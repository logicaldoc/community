package com.logicaldoc.core.job;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.lang3.StringUtils;
import org.quartz.CronScheduleBuilder;
import org.quartz.JobBuilder;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.JobKey;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleScheduleBuilder;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.quartz.TriggerKey;
import org.quartz.impl.matchers.GroupMatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.config.ContextProperties;

/**
 * Facade on Quartz scheduler
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public class JobManager {

	public static final String TENANT_ID = "tenantId";

	public static final String MISSFIRE_RUNNOW = "runnow";

	public static final String MISSFIRE_IGNORE = "ignore";

	protected static Logger log = LoggerFactory.getLogger(JobManager.class);

	@Resource(name = "Scheduler")
	private Scheduler scheduler;

	@Resource(name = "ContextProperties")
	private ContextProperties config;

	/**
	 * Schedules a new Job
	 * 
	 * @param job the Job to schedule
	 * @param dictionary map of data to assign at fire-time to the Job
	 * @param triggers they can be a Date(instant to fire the job) or a
	 *        String(cron expression)
	 * 
	 * @throws SchedulerException error in the scheduler
	 */
	public void schedule(AbstractJob job, Map<String, Object> dictionary, Object... triggers)
			throws SchedulerException {
		Map<Object, Map<String, Object>> trgs = new HashMap<>();
		for (Object trigger : triggers) {
			trgs.put(trigger, new HashMap<>());
		}
		schedule(job, dictionary, trgs);
	}

	/**
	 * Schedules a new Job
	 * 
	 * @param job the Job to schedule
	 * @param dictionary map of data to assign at fire-time to the Job
	 * @param triggers map of triggers, the key is a Date(instant to fire the
	 *        job) or a String(cron expression) and the value is the map of data
	 *        to pass at fire-time to the trigger
	 * 
	 * @throws SchedulerException error in the scheduler
	 */
	public void schedule(AbstractJob job, Map<String, Object> dictionary, Map<Object, Map<String, Object>> triggers)
			throws SchedulerException {
		if (job.getTenantId() != null && !dictionary.containsKey(TENANT_ID))
			dictionary.put(TENANT_ID, job.getTenantId());

		JobKey jobKey = JobKey.jobKey(job.getName(), job.getGroup());
		JobDetail jobDetail = scheduler.getJobDetail(jobKey);
		if (jobDetail != null) {
			// Delete the job and all it's triggers that may altrady exist
			scheduler.deleteJob(jobKey);
		}

		jobDetail = JobBuilder.newJob(job.getClass()).withIdentity(job.getName(), job.getGroup())
				.withDescription(job.getDescription() != null ? job.getDescription() : "")
				.usingJobData(new JobDataMap(dictionary)).build();

		Set<Trigger> trgs = new HashSet<>();
		for (Object triggerSpec : triggers.keySet()) {
			Trigger trig = prepareTrigger(job, triggerSpec, triggers);
			trgs.add(trig);
		}

		scheduler.scheduleJob(jobDetail, trgs, true);
	}

	private Trigger prepareTrigger(AbstractJob job, Object triggerSpec, Map<Object, Map<String, Object>> triggersMap) {
		Trigger trig = null;

		if (!triggersMap.get(triggerSpec).containsKey(TENANT_ID))
			triggersMap.get(triggerSpec).put(TENANT_ID, job.getTenantId());

		if (triggerSpec instanceof Date) {
			// The job must be fired on a specific data

			SimpleScheduleBuilder schedule = SimpleScheduleBuilder.simpleSchedule();
			if (MISSFIRE_RUNNOW.equals(getMissfireInstruction(job.getGroup())))
				schedule = schedule.withMisfireHandlingInstructionFireNow();
			else
				schedule = schedule.withMisfireHandlingInstructionIgnoreMisfires();

			SimpleDateFormat df = new SimpleDateFormat("yyyyMMdd_HHmmss");
			trig = TriggerBuilder.newTrigger()
					.withIdentity(job.getName() + "-" + df.format(triggerSpec), job.getGroup())
					.usingJobData(new JobDataMap(triggersMap.get(triggerSpec))).startAt((Date) triggerSpec)
					.withSchedule(schedule).build();
		} else if (triggerSpec instanceof String) {
			// The job must be fired on a specific data
			CronScheduleBuilder schedule = CronScheduleBuilder.cronSchedule((String) triggerSpec);
			if (MISSFIRE_RUNNOW.equals(getMissfireInstruction(job.getGroup())))
				schedule = schedule.withMisfireHandlingInstructionFireAndProceed();
			else
				schedule = schedule.withMisfireHandlingInstructionDoNothing();

			trig = TriggerBuilder.newTrigger().withIdentity(job.getName() + "-" + triggerSpec, job.getGroup())
					.usingJobData(new JobDataMap(triggersMap.get(triggerSpec))).withSchedule(schedule).build();
		} else {
			log.warn("Skipping trigger {} because not a string nor a date", triggerSpec);
		}

		return trig;
	}

	/**
	 * What policy to use in case of missfire
	 * 
	 * @param group the group name
	 * 
	 * @return JobManager.MISSFIRE_RUNNOW or JobManager.MISSFIRE_IGNORE
	 */
	public String getMissfireInstruction(String group) {
		return config.getProperty("job.calendar.missfire", MISSFIRE_RUNNOW);
	}

	/**
	 * Max number of days to look in the past for missfired triggers
	 * 
	 * @param group the group name
	 * 
	 * @return max number of days
	 */
	public int getMissfireMax(String group) {
		return config.getInt("job.calendar.missfire.max", 2);
	}

	/**
	 * Retrieves all the job groups
	 * 
	 * @return list of groups
	 * 
	 * @throws SchedulerException error in the scheduler
	 */
	public List<String> getGroups() throws SchedulerException {
		return scheduler.getJobGroupNames();
	}

	/**
	 * Retrieves the jobs
	 * 
	 * @param group name of the group or null to retrieve all the jobs
	 * @param tenantId filter on the tenant
	 * 
	 * @return list of job details ordered by group and name
	 * 
	 * @throws SchedulerException error in the scheduler
	 */
	public List<JobDetail> getJobs(String group, Long tenantId) throws SchedulerException {
		Set<JobKey> jobKeys = scheduler.getJobKeys(
				StringUtils.isNotEmpty(group) ? GroupMatcher.groupEquals(group) : GroupMatcher.anyJobGroup());

		List<JobDetail> jobs = new ArrayList<>();
		for (JobKey key : jobKeys) {
			JobDetail job = getJob(key.getName(), key.getGroup());
			if (tenantId == null || tenantId.equals(job.getJobDataMap().get(TENANT_ID)))
				jobs.add(job);
		}

		jobs.sort((JobDetail o1, JobDetail o2) -> {
			int cmp = o1.getKey().getGroup().compareTo(o2.getKey().getGroup());
			if (cmp == 0)
				cmp = o1.getKey().getName().compareTo(o2.getKey().getName());
			return cmp;
		});
		return jobs;
	}

	public JobDetail getJob(String name, String group) throws SchedulerException {
		return scheduler.getJobDetail(JobKey.jobKey(name, group));
	}

	public List<Trigger> getTriggersOfJob(String name, String group) throws SchedulerException {
		return scheduler.getTriggersOfJob(JobKey.jobKey(name, group)).stream().collect(Collectors.toList());
	}

	public void unscheduleJob(String name, String group) throws SchedulerException {
		if (scheduler != null) {
			JobDetail job = scheduler.getJobDetail(JobKey.jobKey(name, group));
			if (job != null)
				scheduler.deleteJob(JobKey.jobKey(name, group));
		}
	}

	public void unscheduleTrigger(String name, String group) throws SchedulerException {
		if (scheduler != null) {
			Trigger trigger = scheduler.getTrigger(TriggerKey.triggerKey(name, group));
			if (trigger != null)
				scheduler.unscheduleJob(TriggerKey.triggerKey(name, group));
		}
	}

	/**
	 * Retrieves the triggers
	 * 
	 * @param group name of the group or null to retrieve all the triggers
	 * @param tenantId filter on the tenant
	 * 
	 * @return list of triggers ordered by group and name
	 * 
	 * @throws SchedulerException error in the scheduler
	 */
	public List<Trigger> getTriggers(String group, Long tenantId) throws SchedulerException {
		Set<TriggerKey> triggerKeys = scheduler
				.getTriggerKeys(StringUtils.isNotEmpty(group) ? GroupMatcher.triggerGroupEquals(group)
						: GroupMatcher.anyTriggerGroup());

		List<Trigger> triggers = new ArrayList<>();
		for (TriggerKey key : triggerKeys) {
			Trigger trigger = getTrigger(key.getName(), key.getGroup());
			if (tenantId == null || tenantId.equals(trigger.getJobDataMap().get(TENANT_ID)))
				triggers.add(trigger);
		}

		triggers.sort((Trigger o1, Trigger o2) -> {
			int cmp = o1.getKey().getGroup().compareTo(o2.getKey().getGroup());
			if (cmp == 0)
				cmp = o1.getKey().getName().compareTo(o2.getKey().getName());
			return cmp;
		});
		return triggers;
	}

	public Trigger getTrigger(String name, String group) throws SchedulerException {
		return scheduler.getTrigger(TriggerKey.triggerKey(name, group));
	}

	public void setScheduler(Scheduler scheduler) {
		this.scheduler = scheduler;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}
}