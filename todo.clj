#!/usr/bin/env bb

(ns todo.core
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [babashka.fs :as fs]
            [org.httpkit.server :as server]))

(def openai-api-key (or (System/getenv "OPENAI_API_KEY") "your-openai-api-key"))
(def todo-file "todos.edn")

(defn load-todos []
  (let [todos (if (fs/exists? todo-file)
                (edn/read-string (slurp todo-file))
                [])]
    (if (vector? todos) todos (vec todos))))

(defn save-todos [todos]
  (spit todo-file (pr-str todos)))

(defn add-task [args]
  (let [description (get args :description)
        digest (get args :digest)
        due-date (get args :due-date)
        todos (load-todos)
        task {:id (inc (count todos))
              :description (str description)
              :digest (str digest)
              :due-date (str due-date)
              :completed false}]
    (save-todos (conj todos task))
    (str "Added task: <span style=\"color: black\">" digest "</span> (due: <span style=\"color: red\">" due-date "</span>)")))

(defn list-tasks [_]
  (let [todos (load-todos)]
    (if (empty? todos)
      "No tasks found."
      (str "## Your Tasks\n" (str/join "\n" (map-indexed #(str "- <span style=\"color: black\">" (:digest %2) "</span> [<span style=\"color: green\">" (:description %2) "</span>] [due: <span style=\"color: red\">" (:due-date %2) "</span>, <span style=\"color: brown\">" (if (:completed %2) "completed" "pending") "</span>]") todos))))))

(defn update-task [args]
  (let [old-digest (get args :old-digest)
        new-due-date (get args :new-due-date)
        todos (load-todos)
        task-index (first (keep-indexed #(when (and (:digest %2) (= (str/lower-case (:digest %2)) (str/lower-case old-digest))) %1) todos))]
    (if task-index
      (let [updated-todos (update todos task-index merge
                                  {:due-date (str new-due-date)})
            updated-task (nth updated-todos task-index)]
        (println "DEBUG: updated-task =" updated-task)
        (save-todos updated-todos)
        (str "Updated task: <span style=\"color: black\">" old-digest "</span> [due: <span style=\"color: red\">" (:due-date updated-task) "</span>]"))
      (str "Task not found: " old-digest))))

(defn delete-task [args]
  (let [digest (get args :digest)
        todos (load-todos)
        new-todos (remove #(and (:digest %) (= (str/lower-case (:digest %)) (str/lower-case digest))) todos)]
    (save-todos new-todos)
    (if (< (count new-todos) (count todos))
      (str "Deleted task: <span style=\"color: black\">" digest "</span>")
      (str "Task not found: " digest))))

(defn complete-task [args]
  (let [digest (get args :digest)
        todos (load-todos)]
    (println "DEBUG: digest =" digest "type =" (type digest))
    (println "DEBUG: todos =" todos "type =" (type todos))
    (let [task-index (first (keep-indexed #(when (and (:digest %2) (= (str/lower-case (:digest %2)) (str/lower-case digest))) %1) todos))]
      (if task-index
        (let [updated-todos (update todos task-index assoc :completed true)]
          (save-todos updated-todos)
          (str "Completed task: <span style=\"color: black\">" digest "</span>"))
        (str "Task not found: " digest)))))

(defn ask-todo [input]
  (let [current-date (java.time.LocalDate/now)
        messages [{:role "system" :content (str "Today is " current-date". You manage a todo list. Match tasks ignoring case. For 'add', use 'add_task' with 'description' as the full task name, 'digest' as a 1-2 word summary, and 'due-date'. For 'delete' or 'to delete [digest]', use 'delete_task' with 'digest'. For 'complete' or 'to complete [digest]', use 'complete_task' with 'digest'. For 'update', 'to update', 'change', or 'to change [digest] to [new-due-date]', use 'update_task' with 'old-digest' as the current task digest and 'new-due-date' as the new date, keeping the full description unchanged unless explicitly specified. Use 'list_tasks' to show tasks.")}
                  {:role "user" :content input}]
        tools-map {:add_task {:type "function"
                              :function {:name "add_task"
                                         :description "Adds a new task to the todo list"
                                         :strict true
                                         :parameters {:type "object"
                                                      :properties {:description {:type "string" :description "The full description of the task"}
                                                                   :digest {:type "string" :description "A 1-2 word summary of the task"}
                                                                   :due-date {:type "string" :description "The deadline for completing the task, formatted as a string"}}
                                                      :required ["description" "digest" "due-date"]
                                                      :additionalProperties false}}}
                   :list_tasks {:type "function"
                                :function {:name "list_tasks"
                                           :description "Retrieves and lists all tasks with their descriptions, due dates, and completion status."
                                           :strict true
                                           :parameters {:type "object"
                                                        :properties {}
                                                        :required []
                                                        :additionalProperties false}}}
                   :update_task {:type "function"
                                 :function {:name "update_task"
                                            :description "Updates the due date of an existing task in the todo list."
                                            :strict true
                                            :parameters {:type "object"
                                                         :properties {:old-digest {:type "string" :description "The current digest of the task to be updated"}
                                                                      :new-due-date {:type "string" :description "The new due date for the task"}}
                                                         :required ["old-digest" "new-due-date"]
                                                         :additionalProperties false}}}
                   :delete_task {:type "function"
                                 :function {:name "delete_task"
                                            :description "Deletes a task by its digest from the todo list"
                                            :strict true
                                            :parameters {:type "object"
                                                         :properties {:digest {:type "string" :description "The digest of the task to delete"}}
                                                         :required ["digest"]
                                                         :additionalProperties false}}}
                   :complete_task {:type "function"
                                   :function {:name "complete_task"
                                              :description "Marks a specified task as completed in the todo list."
                                              :strict true
                                              :parameters {:type "object"
                                                           :properties {:digest {:type "string" :description "The digest of the task to be marked as completed"}}
                                                           :required ["digest"]
                                                           :additionalProperties false}}}}
        tools (vals tools-map)
        response (http/post "https://vajra.one:8848/chat/completions"
                           {:headers {"Authorization" (str "Bearer " openai-api-key)
                                      "Content-Type" "application/json"}
                            :body (json/generate-string {:model "gpt-4o"
                                                         :messages messages
                                                         :tools tools
                                                         :tool_choice "auto"})})
        parsed (json/parse-string (:body response) true)
        message (get-in parsed [:choices 0 :message])]
    (println "DEBUG: message =" message)
    (if-let [tool-calls (:tool_calls message)]
      (let [result (str/join "\n" (map (fn [call]
                                         (let [fname (get-in call [:function :name])
                                               args (json/parse-string (get-in call [:function :arguments]) true)
                                               schema (get tools-map (keyword fname))]
                                           (println "DEBUG: calling" fname "with" args "type of args =" (type args))
                                           (println "DEBUG: schema for" fname "=" (json/generate-string schema {:pretty true}))
                                           (case fname
                                             "add_task" (add-task args)
                                             "list_tasks" (list-tasks args)
                                             "update_task" (update-task args)
                                             "delete_task" (delete-task args)
                                             "complete_task" (complete-task args)
                                             (str "Unknown function: " fname))))
                                       (if (sequential? tool-calls) tool-calls [tool-calls])))]
        (println "DEBUG: tool_calls =" tool-calls)
        result)
      (let [content (:content message)]
        (println "DEBUG: no tool calls, content =" content)
        content))))

(defn handler [req]
  (println "Received request:" req)
  (cond
    (= (:uri req) "/")
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (slurp "index.html")}

    (= (:uri req) "/api")
    (if (= (:request-method req) :post)
      (let [body (if (:body req)
                   (json/parse-string (slurp (:body req)) true)
                   {:message ""})
            input (:message body)]
        {:status 200
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string {:response (ask-todo (or input "No input provided"))})})
      {:status 405
       :headers {"Content-Type" "text/plain"}
       :body "Method Not Allowed - Use POST"})

    :else
    {:status 404
     :headers {"Content-Type" "text/plain"}
     :body "Not Found"}))

(server/run-server handler {:port 8080})
(println "Server running at http://localhost:8080")
@(promise)
