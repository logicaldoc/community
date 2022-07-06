package com.logicaldoc.core.metadata;

import java.util.HashSet;
import java.util.Set;

/**
 * A template collects a set of attributesets ant is itself an extensible
 * object.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class Template extends ExtensibleObject {

	private static final long serialVersionUID = 1L;

	public static int TYPE_DEFAULT = 0;

	private String name;

	private String description;

	private int readonly = 0;

	private int type = TYPE_DEFAULT;

	private String validation;

	protected Set<TemplateGroup> templateGroups = new HashSet<TemplateGroup>();

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public int getReadonly() {
		return readonly;
	}

	public void setReadonly(int readonly) {
		this.readonly = readonly;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public Set<TemplateGroup> getTemplateGroups() {
		return templateGroups;
	}

	public void setTemplateGroups(Set<TemplateGroup> templateGroups) {
		this.templateGroups = templateGroups;
	}

	/**
	 * Adds a new element, substituting a previous one with the same groupId.
	 * 
	 * @param tg the template group
	 */
	public void addTemplateGroup(TemplateGroup tg) {
		TemplateGroup m = getWorkflowGroup(tg.getGroupId());
		getTemplateGroups().remove(m);
		getTemplateGroups().add(tg);
	}

	public TemplateGroup getWorkflowGroup(long groupId) {
		for (TemplateGroup tg : templateGroups) {
			if (tg.getGroupId() == groupId)
				return tg;
		}
		return null;
	}

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
	}
}