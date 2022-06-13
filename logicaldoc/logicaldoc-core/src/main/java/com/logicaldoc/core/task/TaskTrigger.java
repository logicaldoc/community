package com.logicaldoc.core.task;

import org.quartz.CronTrigger;
import org.quartz.JobDetail;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.scheduling.quartz.CronTriggerFactoryBean;
import org.springframework.scheduling.quartz.SimpleTriggerFactoryBean;

import com.logicaldoc.util.config.ContextProperties;

/**
 * This trigger wraps both a SimpleTrigger and a CronTrigger
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class TaskTrigger implements FactoryBean<Trigger>, BeanNameAware, InitializingBean {

	private SimpleTriggerFactoryBean simpleTrigger = null;

	private CronTriggerFactoryBean cronTrigger = null;

	public static String MODE_CRON = "cron";

	public static String MODE_SIMPLE = "simple";

	private ContextProperties config;

	private Task task;

	private JobDetail jobDetail;

	private String beanName;

	private TaskTrigger() {
		super();
	}

	public Task getTask() {
		return task;
	}

	public String getName() {
		if (task != null)
			return task.getName();
		else
			return "";
	}

	public void setTask(Task task) {
		this.task = task;
	}

	public SimpleTriggerFactoryBean getSimpleTrigger() {
		return simpleTrigger;
	}

	public CronTriggerFactoryBean getCronTrigger() {
		return cronTrigger;
	}

	public void setJobDetail(JobDetail jobDetail) {
		this.jobDetail = jobDetail;
	}

	@Override
	public void afterPropertiesSet() throws Exception {

	}

	@Override
	public void setBeanName(String beanName) {
		this.beanName = beanName;
	}

	@Override
	public Trigger getObject() {
		if ("simple".equals(config.getProperty("schedule.mode." + getName()))) {
			if (simpleTrigger == null) {
				simpleTrigger = new SimpleTriggerFactoryBean();
				simpleTrigger.setName(getName());
				simpleTrigger.setRepeatInterval(Long.parseLong(config.getProperty("schedule.interval." + getName())));
				simpleTrigger.setStartDelay(Long.parseLong(config.getProperty("schedule.delay." + getName())));
				simpleTrigger.setJobDetail(jobDetail);
				simpleTrigger.afterPropertiesSet();
			}
			cronTrigger = null;
			return simpleTrigger.getObject();
		} else {
			if (cronTrigger == null)
				try {
					cronTrigger = new CronTriggerFactoryBean();
					cronTrigger.setName(getName());
					cronTrigger.setCronExpression(config.getProperty("schedule.cron." + getName()));
					cronTrigger.setJobDetail(jobDetail);
					cronTrigger.afterPropertiesSet();
				} catch (Throwable e) {

				}
			simpleTrigger = null;
			return cronTrigger.getObject();
		}
	}

	@Override
	public Class<?> getObjectType() {
		try {
			if ("simple".equals(config.getProperty("schedule.mode." + getName())))
				return SimpleTrigger.class;
			else
				return CronTrigger.class;
		} catch (Throwable t) {
			return SimpleTrigger.class;
		}
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	public void reload() {
		this.cronTrigger = null;
		this.simpleTrigger = null;
		getObject();
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	public long getRepeatInterval() {
		if (simpleTrigger != null)
			return simpleTrigger.getObject().getRepeatInterval();
		else
			return -1;
	}

	public JobDetail getJobDetail() {
		return jobDetail;
	}
}
