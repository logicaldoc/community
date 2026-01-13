package com.logicaldoc.core.metadata;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Secure;

import jakarta.persistence.Cacheable;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.MapKeyColumn;
import jakarta.persistence.OrderBy;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

/**
 * A template collects a set of attributesets ant is itself an extensible
 * object.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@Entity
@Table(name = "ld_template")
@Cacheable
public class Template extends AbstractAttributeSet implements Secure<AccessControlEntry> {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_initialization")
	private String initialization;

	@Column(name = "ld_validation")
	private String validation;

	@Transient
	private Set<AccessControlEntry> accessControlList = new HashSet<>();

	@ElementCollection
	@CollectionTable(name = "ld_template_ext", joinColumns = @JoinColumn(name = "ld_templateid"))
	@MapKeyColumn(name = "ld_name", length = 255)
	@OrderBy("ld_position ASC, ld_name ASC")
	private Map<String, Attribute> templateAttributes = new HashMap<>();

	@Override
	public Map<String, Attribute> getTemplateAttributes() {
		return templateAttributes;
	}

	@Override
	public void setTemplateAttributes(Map<String, Attribute> templateAttributes) {
		this.templateAttributes = templateAttributes;
	}

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
	}

	public String getInitialization() {
		return initialization;
	}

	public void setInitialization(String initialization) {
		this.initialization = initialization;
	}

	@Override
	public Set<AccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	@Override
	public void setAccessControlList(Set<AccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	@Override
	public AccessControlEntry getAccessControlEntry(long groupId) {
		return getAccessControlList().stream().filter(ace -> ace.getGroupId() == groupId).findFirst().orElse(null);
	}
	
	@Override
	public Set<AccessControlEntry> getAccessControlEntries(Set<Long> groupIds) {
		return getAccessControlList().stream().filter(ace -> groupIds.contains(ace.getGroupId()))
				.collect(Collectors.toSet());
	}

	@Override
	public void addAccessControlEntry(AccessControlEntry ace) {
		if (!getAccessControlList().add(ace)) {
			getAccessControlList().remove(ace);
			getAccessControlList().add(ace);
		}
	}
	
	@Override
	public void removeAccessControlEntries(long groupId) {
		getAccessControlList().removeIf(ace -> ace.getGroupId() == groupId);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((validation == null) ? 0 : validation.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		Template other = (Template) obj;
		if (validation == null) {
			if (other.validation != null)
				return false;
		} else if (!validation.equals(other.validation))
			return false;
		return true;
	}
}