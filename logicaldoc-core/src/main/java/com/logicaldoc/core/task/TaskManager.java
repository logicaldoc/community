package com.logicaldoc.core.task;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;

import com.logicaldoc.core.document.TagsProcessor;
import com.logicaldoc.core.searchengine.IndexOptimizer;
import com.logicaldoc.core.searchengine.IndexerTask;
import com.logicaldoc.util.Context;
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

	/**
	 * Retrieves the collection of all defined tasks: the ones enlisted in the
	 * Task extension point.
	 * 
	 * @param context the application context to use
	 * 
	 * @return collection of tasks
	 */
	Collection<Task> getTasks(ApplicationContext context) {
		List<Task> tasks = new ArrayList<Task>();
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
	 * Retrieves the collection of all defined tasks: the ones enlisted in the
	 * Task extension point.
	 * 
	 * @return collection of tasks
	 */
	public Collection<Task> getTasks() {
		Context context=Context.get();
		
		List<Task> tasks = new ArrayList<Task>();		
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
			synchronized (this) {
				try {
					wait(500);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		}
		log.info("All tasks are idle");
	}
}