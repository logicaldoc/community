package com.logicaldoc.core.metadata;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.persistence.Cacheable;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.MapKeyColumn;
import javax.persistence.OrderBy;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Secure;

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
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class Template extends AbstractAttributeSet implements Secure<AccessControlEntry> {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_validation")
	private String validation;

	@Transient
	private Set<AccessControlEntry> accessControlList = new HashSet<>();

	@ElementCollection
	@CollectionTable(name = "ld_template_ext", joinColumns = @JoinColumn(name = "ld_templateid"))
	@MapKeyColumn(name = "ld_name", length = 255)
	@OrderBy("ld_position ASC, ld_name ASC")
	private Map<String, TemplateAttribute> templateAttributes = new HashMap<>();

	@Override
	public Map<String, TemplateAttribute> getTemplateAttributes() {
		return templateAttributes;
	}

	@Override
	public void setTemplateAttributes(Map<String, TemplateAttribute> templateAttributes) {
		this.templateAttributes = templateAttributes;
	}

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
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
	public void addAccessControlEntry(AccessControlEntry ace) {
		if (!getAccessControlList().add(ace)) {
			getAccessControlList().remove(ace);
			getAccessControlList().add(ace);
		}
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