package com.logicaldoc.core.task;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
public class SchedulerFactoryBean extends org.springframework.scheduling.quartz.SchedulerFactoryBean
		implements ApplicationContextAware {

	protected static Logger log = LoggerFactory.getLogger(SchedulerFactoryBean.class);

	@Override
	public void start() throws SchedulingException {
		if (RunLevel.current().aspectEnabled("scheduledTasks"))
			super.start();
		else
			log.warn("Aspect disabled");
	}

	@Override
	public Scheduler getObject() {
		if (RunLevel.current().aspectEnabled("scheduledTasks"))
			return super.getObject();
		else {
			log.debug("Aspect disabled");
			return null;
		}
	}

	@Override
	public boolean isAutoStartup() {
		if (RunLevel.current().aspectEnabled("scheduledTasks"))
			return super.isAutoStartup();
		else
			return false;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		if (RunLevel.current().aspectEnabled("scheduledTasks"))
			super.afterPropertiesSet();
		else
			log.warn("Aspect disabled");
	}

	@Override
	public void destroy() throws SchedulerException {
		super.destroy();
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) {
		super.setApplicationContext(applicationContext);

		List<Trigger> triggers = new ArrayList<Trigger>();

		TaskManager manager = (TaskManager) applicationContext.getBean("TaskManager");
		Collection<Task> tasks = manager.getTasks(applicationContext);
		for (Task task : tasks) {
			String name = task.getName();
			Trigger trigger = (Trigger) applicationContext.getBean(name + "Trigger");
			if (trigger != null)
				triggers.add(trigger);
			else
				log.warn("Cannot schedule task {}", name);
		}

		// Some default triggers
		Trigger trigger = (Trigger) applicationContext.getBean("TempFolderCleaner");
		if (trigger != null)
			triggers.add(trigger);
		trigger = (Trigger) applicationContext.getBean("GarbageCollector");
		if (trigger != null)
			triggers.add(trigger);
		
		
		if (!triggers.isEmpty())
			setTriggers(triggers.toArray(new Trigger[0]));
	}
}