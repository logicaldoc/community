package com.logicaldoc.core.task;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.quartz.Calendar;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobKey;
import org.quartz.ListenerManager;
import org.quartz.Scheduler;
import org.quartz.SchedulerContext;
import org.quartz.SchedulerException;
import org.quartz.SchedulerMetaData;
import org.quartz.Trigger;
import org.quartz.Trigger.TriggerState;
import org.quartz.TriggerKey;
import org.quartz.UnableToInterruptJobException;
import org.quartz.impl.matchers.GroupMatcher;
import org.quartz.spi.JobFactory;

public class DummyScheduler implements Scheduler {

	@Override
	public String getSchedulerName() throws SchedulerException {
		return null;
	}

	@Override
	public String getSchedulerInstanceId() throws SchedulerException {
		return null;
	}

	@Override
	public SchedulerContext getContext() throws SchedulerException {
		return new SchedulerContext();
	}

	@Override
	public void start() throws SchedulerException {
		// Do nothing
	}

	@Override
	public void startDelayed(int seconds) throws SchedulerException {
		// Do nothing
	}

	@Override
	public boolean isStarted() throws SchedulerException {
		return false;
	}

	@Override
	public void standby() throws SchedulerException {
		// Do nothing
	}

	@Override
	public boolean isInStandbyMode() throws SchedulerException {
		return false;
	}

	@Override
	public void shutdown() throws SchedulerException {
		// Do nothing
	}

	@Override
	public void shutdown(boolean waitForJobsToComplete) throws SchedulerException {
		// Do nothing
	}

	@Override
	public boolean isShutdown() throws SchedulerException {
		return false;
	}

	@Override
	public SchedulerMetaData getMetaData() throws SchedulerException {
		return null;
	}

	@Override
	public List<JobExecutionContext> getCurrentlyExecutingJobs() throws SchedulerException {
		return new ArrayList<>();
	}

	@Override
	public void setJobFactory(JobFactory factory) throws SchedulerException {
		// Do nothing
	}

	@Override
	public ListenerManager getListenerManager() throws SchedulerException {
		return null;
	}

	@Override
	public Date scheduleJob(JobDetail jobDetail, Trigger trigger) throws SchedulerException {
		return null;
	}

	@Override
	public Date scheduleJob(Trigger trigger) throws SchedulerException {
		return null;
	}

	@Override
	public void scheduleJobs(Map<JobDetail, Set<? extends Trigger>> triggersAndJobs, boolean replace)
			throws SchedulerException {
		// Do nothing
	}

	@Override
	public void scheduleJob(JobDetail jobDetail, Set<? extends Trigger> triggersForJob, boolean replace)
			throws SchedulerException {
		// Do nothing
	}

	@Override
	public boolean unscheduleJob(TriggerKey triggerKey) throws SchedulerException {
		return false;
	}

	@Override
	public boolean unscheduleJobs(List<TriggerKey> triggerKeys) throws SchedulerException {
		return false;
	}

	@Override
	public Date rescheduleJob(TriggerKey triggerKey, Trigger newTrigger) throws SchedulerException {
		return null;
	}

	@Override
	public void addJob(JobDetail jobDetail, boolean replace) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void addJob(JobDetail jobDetail, boolean replace, boolean storeNonDurableWhileAwaitingScheduling)
			throws SchedulerException {
		// Do nothing
	}

	@Override
	public boolean deleteJob(JobKey jobKey) throws SchedulerException {
		return false;
	}

	@Override
	public boolean deleteJobs(List<JobKey> jobKeys) throws SchedulerException {
		return false;
	}

	@Override
	public void triggerJob(JobKey jobKey) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void triggerJob(JobKey jobKey, JobDataMap data) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void pauseJob(JobKey jobKey) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void pauseJobs(GroupMatcher<JobKey> matcher) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void pauseTrigger(TriggerKey triggerKey) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void pauseTriggers(GroupMatcher<TriggerKey> matcher) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void resumeJob(JobKey jobKey) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void resumeJobs(GroupMatcher<JobKey> matcher) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void resumeTrigger(TriggerKey triggerKey) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void resumeTriggers(GroupMatcher<TriggerKey> matcher) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void pauseAll() throws SchedulerException {
		// Do nothing
	}

	@Override
	public void resumeAll() throws SchedulerException {
		// Do nothing
	}

	@Override
	public List<String> getJobGroupNames() throws SchedulerException {
		return new ArrayList<>();
	}

	@Override
	public Set<JobKey> getJobKeys(GroupMatcher<JobKey> matcher) throws SchedulerException {
		return new HashSet<>();
	}

	@Override
	public List<? extends Trigger> getTriggersOfJob(JobKey jobKey) throws SchedulerException {
		return new ArrayList<>();
	}

	@Override
	public List<String> getTriggerGroupNames() throws SchedulerException {
		return new ArrayList<>();
	}

	@Override
	public Set<TriggerKey> getTriggerKeys(GroupMatcher<TriggerKey> matcher) throws SchedulerException {
		return new HashSet<>();
	}

	@Override
	public Set<String> getPausedTriggerGroups() throws SchedulerException {
		return new HashSet<>();
	}

	@Override
	public JobDetail getJobDetail(JobKey jobKey) throws SchedulerException {
		return null;
	}

	@Override
	public Trigger getTrigger(TriggerKey triggerKey) throws SchedulerException {
		return null;
	}

	@Override
	public TriggerState getTriggerState(TriggerKey triggerKey) throws SchedulerException {
		return null;
	}

	@Override
	public void resetTriggerFromErrorState(TriggerKey triggerKey) throws SchedulerException {
		// Do nothing
	}

	@Override
	public void addCalendar(String calName, Calendar calendar, boolean replace, boolean updateTriggers)
			throws SchedulerException {
		// Do nothing
	}

	@Override
	public boolean deleteCalendar(String calName) throws SchedulerException {
		return false;
	}

	@Override
	public Calendar getCalendar(String calName) throws SchedulerException {
		return null;
	}

	@Override
	public List<String> getCalendarNames() throws SchedulerException {
		return new ArrayList<>();
	}

	@Override
	public boolean interrupt(JobKey jobKey) throws UnableToInterruptJobException {
		return false;
	}

	@Override
	public boolean interrupt(String fireInstanceId) throws UnableToInterruptJobException {
		return false;
	}

	@Override
	public boolean checkExists(JobKey jobKey) throws SchedulerException {
		return false;
	}

	@Override
	public boolean checkExists(TriggerKey triggerKey) throws SchedulerException {
		return false;
	}

	@Override
	public void clear() throws SchedulerException {
		// Do nothing
	}
}