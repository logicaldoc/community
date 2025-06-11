package com.logicaldoc.core.metadata;

import java.util.HashMap;
import java.util.Map;

import jakarta.persistence.Cacheable;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.MapKeyColumn;
import jakarta.persistence.OrderBy;
import jakarta.persistence.Table;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

/**
 * Represents a set of attributes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
@Entity
@Table(name = "ld_attributeset")
@Cacheable
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class AttributeSet extends AbstractAttributeSet {

	private static final long serialVersionUID = 1L;
	
	@ElementCollection(fetch = FetchType.EAGER)
	@CollectionTable(name = "ld_attributeset_ext", joinColumns = @JoinColumn(name = "ld_attsetid"))
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

}