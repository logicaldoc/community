package com.logicaldoc.core.task;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.quartz.Scheduler;
import org.quartz.Trigger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.scheduling.SchedulingException;

import com.logicaldoc.core.RunLevel;

/**
 * Scheduler factory to enable/disable the scheduler depending on the
 * runlevel. @see org.springframework.scheduling.quartz.SchedulerFactoryBean
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class DefaultSchedulerFactory extends org.springframework.scheduling.quartz.SchedulerFactoryBean
		implements ApplicationContextAware {

	private static final String ASPECT_DISABLED = "Aspect disabled";

	private static final String SCHEDULED_TASKS = "scheduledTasks";

	protected static Logger log = LoggerFactory.getLogger(DefaultSchedulerFactory.class);

	@Override
	public void start() throws SchedulingException {
		if (RunLevel.current().aspectEnabled(SCHEDULED_TASKS)) {
			super.start();
		} else
			log.warn(ASPECT_DISABLED);
	}

	@Override
	public Scheduler getObject() {
		if (RunLevel.current().aspectEnabled(SCHEDULED_TASKS))
			return super.getObject();
		else {
			log.debug(ASPECT_DISABLED);
			return null;
		}
	}

	@Override
	public boolean isAutoStartup() {
		if (RunLevel.current().aspectEnabled(SCHEDULED_TASKS))
			return super.isAutoStartup();
		else
			return false;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		if (RunLevel.current().aspectEnabled(SCHEDULED_TASKS))
			super.afterPropertiesSet();
		else
			log.warn(ASPECT_DISABLED);
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) {
		super.setApplicationContext(applicationContext);

		List<Trigger> triggers = new ArrayList<>();

		TaskManager manager = (TaskManager) applicationContext.getBean("TaskManager");
		Collection<Task> tasks = manager.getTasks(applicationContext);
		for (Task task : tasks) {
			String name = task.getName();
			Object trigger = applicationContext.getBean(name + "Trigger");
			if (trigger instanceof Trigger) {
				triggers.add((Trigger) trigger);
			} else
				log.warn("Cannot schedule task {}", name);
		}

		// Some default triggers
		try {
			triggers.add((Trigger) applicationContext.getBean("TempFolderCleaner"));
		} catch (BeansException e) {
			// Nothing to do
		}

		if (!triggers.isEmpty())
			setTriggers(triggers.toArray(new Trigger[0]));
	}
}