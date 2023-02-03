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
public class Template extends AbstractAttributeSet {

	private static final long serialVersionUID = 1L;

	private String validation;

	private Set<TemplateGroup> templateGroups = new HashSet<>();

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