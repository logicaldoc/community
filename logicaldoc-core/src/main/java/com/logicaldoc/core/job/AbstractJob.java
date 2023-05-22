package com.logicaldoc.core.job;

/**
 * This represents a unit of work to be scheduled in Quartz
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public abstract class AbstractJob implements org.quartz.Job {

	private String name;

	private String group;

	private String description;

	private Long tenantId;

	protected AbstractJob(String name, String group) {
		this(name, group, null, null);
	}

	protected AbstractJob(String name, String group, Long tenantId) {
		this(name, group, null, tenantId);
	}

	protected AbstractJob(String name, String group, String description) {
		this(name, group, description, null);
	}

	protected AbstractJob(String name, String group, String description, Long tenantId) {
		super();
		this.name = name;
		this.group = group;
		this.description = description;
		this.tenantId = tenantId;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getGroup() {
		return group;
	}

	public void setGroup(String group) {
		this.group = group;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Long getTenantId() {
		return tenantId;
	}

	public void setTenantId(Long tenantId) {
		this.tenantId = tenantId;
	}
}