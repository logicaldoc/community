package com.logicaldoc.core.metadata;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.Cacheable;
import javax.persistence.CollectionTable;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.MapKeyColumn;
import javax.persistence.OrderBy;
import javax.persistence.Table;

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
	private Map<String, TemplateAttribute> templateAttributes = new HashMap<>();

	@Override
	public Map<String, TemplateAttribute> getTemplateAttributes() {
		return templateAttributes;
	}

	@Override
	public void setTemplateAttributes(Map<String, TemplateAttribute> templateAttributes) {
		this.templateAttributes = templateAttributes;
	}

}