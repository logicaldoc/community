package com.logicaldoc.core.task;

import java.io.IOException;
import java.text.ParseException;
import java.util.Date;

import org.quartz.Scheduler;
import org.quartz.Trigger;
import org.quartz.TriggerKey;
import org.quartz.utils.Key;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Scheduling configuration for a Task
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5.0
 */
public class TaskScheduling {

	protected static Logger log = LoggerFactory.getLogger(TaskScheduling.class);

	private String taskName;

	private String mode = TaskTrigger.MODE_SIMPLE;

	private long delay = 1000;

	private long interval = 60000;

	/**
	 * The cron expression
	 */
	private String cronExpression = "* 10 * * * ? *";

	private Date previousFireTime;

	// Maximum duration expressed in seconds
	private long maxLength = -1;

	private boolean enabled = true;

	public TaskScheduling(String taskName) {
		this.taskName = taskName;
	}

	public String getTaskName() {
		return taskName;
	}

	public Date getPreviousFireTime() {
		return previousFireTime;
	}

	public void setPreviousFireTime(Date previousFireTime) {
		this.previousFireTime = previousFireTime;
	}

	/**
	 * The maximum duration expressed in seconds
	 * 
	 * @return the maximpum duration in seconds
	 */
	public long getMaxLength() {
		return maxLength;
	}

	public void setMaxLength(long maxLength) {
		this.maxLength = maxLength;
	}

	public Date getNextFireTime() {
		Date nextFire = null;

		Object trigger = Context.get().getBean(taskName + "Trigger");
		if (trigger instanceof Trigger trgr)
			nextFire = previousFireTime != null ? trgr.getFireTimeAfter(previousFireTime) : trgr.getNextFireTime();

		return nextFire;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * Loads scheduling configurations from persistent store
	 * 
	 * @throws IOException error reading the configuration file
	 * 
	 * @throws ParseException error parsing the scheduling expression
	 */
	public void load() throws IOException, ParseException {
		ContextProperties config = Context.get().getProperties();
		String enbl = config.getProperty("schedule.enabled." + taskName);
		this.enabled = "true".equals(enbl);
		setCronExpression(config.getProperty("schedule.cron." + taskName));
		setMode(config.getProperty("schedule.mode." + taskName));
		try {
			maxLength = Long.parseLong(config.getProperty("schedule.length." + taskName));
			interval = Long.parseLong(config.getProperty("schedule.interval." + taskName));
			delay = Long.parseLong(config.getProperty("schedule.delay." + taskName));
		} catch (Exception e) {
			// Nothing to do
		}
	}

	/**
	 * Saves scheduling configurations in the persistent store
	 * 
	 * @throws IOException raised is an I/O problem occurs
	 * @throws ParseException raised if the scheduling expression is invalid
	 */
	public void save() throws IOException, ParseException {
		Scheduler scheduler = (Scheduler) Context.get().getBean("Scheduler");
		// Use the & prefix to get the factory and not the bean it produces
		TaskTrigger trigger = (TaskTrigger) Context.get().getBean("&" + taskName + "Trigger");
		String expression = getCronExpression();

		ContextProperties config = Context.get().getProperties();
		config.setProperty("schedule.cron." + taskName, expression);
		config.setProperty("schedule.enabled." + taskName, enabled ? "true" : "false");
		config.setProperty("schedule.length." + taskName, Long.toString(maxLength));
		config.setProperty("schedule.mode." + taskName, getMode());
		config.setProperty("schedule.delay." + taskName, Long.toString(delay));
		config.setProperty("schedule.interval." + taskName, Long.toString(interval));
		config.write();

		trigger.reload();

		// Reschedule the job
		try {
			TriggerKey key = new TriggerKey(taskName, Key.DEFAULT_GROUP);
			Date date = scheduler.rescheduleJob(key, trigger.getObject());

			if (date != null)
				log.info("Rescheduled the task {}; next estimated fire time is {}", taskName, date);
			else
				log.warn("Unable to reschedule the task {}", taskName);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		load();
	}

	/**
	 * Checks if the time was expired(a maxLength must be defined)
	 * 
	 * @return if the task execution has expired
	 */
	boolean isExpired() {
		if (previousFireTime == null || getMaxLength() <= 0)
			return false;
		else {
			Date now = new Date();

			// Get msec from each, and subtract.
			long diff = now.getTime() - previousFireTime.getTime();
			if (diff < 0)
				diff = -diff;
			long diffSeconds = diff / 1000;

			return diffSeconds > getMaxLength();
		}
	}

	@Override
	public String toString() {
		return getCronExpression();
	}

	public String getMode() {
		return mode;
	}

	public void setMode(String mode) {
		this.mode = mode;
	}

	public long getDelay() {
		return delay;
	}

	public long getDelaySeconds() {
		return delay / 1000;
	}

	public void setDelay(long delay) {
		this.delay = delay;
	}

	public void setDelaySeconds(long delay) {
		this.delay = delay * 1000;
	}

	public long getInterval() {
		return interval;
	}

	public long getIntervalSeconds() {
		return interval / 1000;
	}

	public void setInterval(long interval) {
		this.interval = interval;
	}

	public void setIntervalSeconds(long interval) {
		this.interval = interval * 1000;
	}

	public String getCronExpression() {
		return cronExpression;
	}

	public void setCronExpression(String cronExpression) {
		this.cronExpression = cronExpression;
	}
}