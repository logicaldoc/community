package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * Representation of a task scheduling
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUIScheduling implements Serializable {

	private static final long serialVersionUID = 1L;

	private String taskName;

	private String mode = "mode";

	private long delay = 1000;

	private long interval = 60000;

	private Date previousFireTime = new Date();

	private Date nextFireTime = new Date();

	// Maximum execution length expressed in seconds
	private long maxLength = -1;

	private boolean enabled = true;

	private boolean simple = true;

	private String cronExpression = "* 10 * * * ? *";
	
	private String lastDuration;

	public GUIScheduling() {
	}

	public GUIScheduling(String taskName) {
		this.taskName = taskName;
	}

	public String getTaskName() {
		return taskName;
	}

	public void setTaskName(String taskName) {
		this.taskName = taskName;
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

	public void setDelay(long delay) {
		this.delay = delay;
	}

	public long getInterval() {
		return interval;
	}

	public void setInterval(long interval) {
		this.interval = interval;
	}

	public Date getPreviousFireTime() {
		return previousFireTime;
	}

	public void setPreviousFireTime(Date previousFireTime) {
		this.previousFireTime = previousFireTime;
	}

	public long getMaxLength() {
		return maxLength;
	}

	public void setMaxLength(long maxLength) {
		this.maxLength = maxLength;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public Date getNextFireTime() {
		return nextFireTime;
	}

	public void setNextFireTime(Date nextFireTime) {
		this.nextFireTime = nextFireTime;
	}

	public boolean isSimple() {
		return simple;
	}

	public void setSimple(boolean simple) {
		this.simple = simple;
	}

	public String getCronExpression() {
		return cronExpression;
	}

	public void setCronExpression(String cronExpression) {
		this.cronExpression = cronExpression;
	}

	public String getLastDuration() {
		return lastDuration;
	}

	public void setLastDuration(String lastDuration) {
		this.lastDuration = lastDuration;
	}
}