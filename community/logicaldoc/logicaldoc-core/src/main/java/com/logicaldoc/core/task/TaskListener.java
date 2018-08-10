package com.logicaldoc.core.task;

import java.util.EventListener;

/**
 * Implementations of this listener are notified when changes occurs in the Task
 * state
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5.0
 */
public interface TaskListener extends EventListener {

	/**
	 * Invoked when the progress changes
	 * 
	 * @param progress the new progress value
	 */
	public void progressChanged(long progress);

	/**
	 * Invoked when the progress changes
	 * 
	 * @param status the new status value(one of STATUS_RUNNING or STATUS_IDLE
	 */
	public void statusChanged(int status);
}
