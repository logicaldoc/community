package com.logicaldoc.webservice.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.logicaldoc.webservice.doc.WSDoc;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A WS Template
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
@XmlRootElement(name = "template")
@XmlType(name = "WSTemplate")
public class WSTemplate implements Serializable {
	@WSDoc(documented = false)
	private static final long serialVersionUID = 1L;

	@WSDoc(description = "unique identifier")
	private long id;

	private String name = "";

	@WSDoc(required = false)
	private String description = "";

	private String lastModified;

	@WSDoc(description = "the last modified date (format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd')", required = false)
	private long docsCount;

	@WSDoc(required = false)
	private String initialization;

	@WSDoc(required = false)
	private String validation;

	@WSDoc(required = false)
	private List<WSAttribute> attributes = new ArrayList<>();

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

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

	public String getLastModified() {
		return lastModified;
	}

	public void setLastModified(String lastModified) {
		this.lastModified = lastModified;
	}

	public long getDocsCount() {
		return docsCount;
	}

	public void setDocsCount(long docsCount) {
		this.docsCount = docsCount;
	}

	public List<WSAttribute> getAttributes() {
		return attributes;
	}

	public WSAttribute getAttribute(String name) {
		if (attributes != null)
			for (WSAttribute attribute : attributes)
				if (attribute.getName().equals(name))
					return attribute;
		return null;
	}

	public void setAttributes(List<WSAttribute> attributes) {
		if (attributes == null)
			this.attributes = new ArrayList<>();
		else
			this.attributes = attributes;
	}

	public Collection<String> listAttributeNames() {
		List<String> names = new ArrayList<>();
		for (WSAttribute att : getAttributes()) {
			names.add(att.getName());
		}
		return names;
	}

	public void addAttribute(WSAttribute att) {
		if (attributes == null)
			attributes = new ArrayList<>();
		attributes.add(att);
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
}
