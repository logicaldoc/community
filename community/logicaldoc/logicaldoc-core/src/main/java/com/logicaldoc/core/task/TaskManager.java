package com.logicaldoc.core.task;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.TagsProcessor;
import com.logicaldoc.core.searchengine.IndexOptimizer;
import com.logicaldoc.core.searchengine.IndexerTask;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextConfigurator;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * A manager that collects all operations about tasks.
 * <p>
 * <b>Important:</b> Only tasks defined in the Task extension point will be
 * considered
 * </p>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5.0
 */
public class TaskManager {
	
	protected static Logger log = LoggerFactory.getLogger(TaskManager.class);

	public void registerTasks() {
		// Acquire the 'Task' extensions of the core plugin and add defined
		// Tasks into the scheduler
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Task");
		for (Extension extension : exts) {
			// Retrieve the task name
			String name = extension.getParameter("name").valueAsString();
			registerTask(name);
		}
	}

	/**
	 * Registers a single task
	 * 
	 * @param taskName The name of the new task to be registered
	 */
	private void registerTask(String taskName) {
		// Add the scheduled trigger
		log.info("Append the task {} to the scheduler", taskName);
		ContextConfigurator contextConfig = new ContextConfigurator();
		contextConfig.addTrigger(taskName + "Trigger");
	}

	/**
	 * Retrieves the collection of all defined tasks: the ones enlisted in the
	 * Task extension point.
	 * 
	 * @return collection of tasks
	 */
	public Collection<Task> getTasks() {
		List<Task> tasks = new ArrayList<Task>();
		Context context = Context.get();
		tasks.add((Task) context.getBean(IndexOptimizer.NAME));
		tasks.add((Task) context.getBean(IndexerTask.NAME));
		tasks.add((Task) context.getBean(TagsProcessor.NAME));

		// Acquire the 'Task' extensions of the core plugin and add iterate over
		// defined tasks
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Task");
		for (Extension extension : exts) {
			// Retrieve the task name
			String name = extension.getParameter("name").valueAsString();
			tasks.add((Task) context.getBean(name));
		}
		return tasks;
	}

	/**
	 * Stops all tasks and waits for them
	 */
	public void stop() {
		Collection<Task> tasks = getTasks();
		for (Task task : tasks)
			task.interrupt();
		boolean running = true;
		while (running) {
			running = false;
			for (Task task : tasks)
				if (task.isRunning()) {
					running = true;
					break;
				}
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
			}
		}
		log.info("All tasks are idle");
	}
}