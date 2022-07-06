package com.logicaldoc.core.task;

import java.util.Date;

import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.SchedulingException;

import com.logicaldoc.core.RunLevel;

/**
 * Scheduler factory to enable/disable the scheduler depending on the runlevel. @see
 * org.springframework.scheduling.quartz.SchedulerFactoryBean
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class SchedulerFactoryBean extends org.springframework.scheduling.quartz.SchedulerFactoryBean {

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
}