package com.logicaldoc.webservice.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.logicaldoc.webservice.doc.WSDoc;

/**
 * A WS Attribute Set
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
@XmlRootElement(name = "attributeSet")
@XmlType(name = "WSAttributeSet")
public class WSAttributeSet implements Serializable {
	@WSDoc(documented = false)
	private static final long serialVersionUID = 1L;

	@WSDoc(description = "unique identifier")
	private long id;

	private String name = "";

	@WSDoc(required = false)
	private String description = "";

	@WSDoc(description = "the last modified date (format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd')", required = false)
	private String lastModified;

	@WSDoc(required = false)
	private WSAttribute[] attributes = new WSAttribute[0];

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

	public WSAttribute[] getAttributes() {
		return attributes;
	}

	public WSAttribute getAttribute(String name) {
		if (attributes != null)
			for (WSAttribute attribute : attributes)
				if (attribute.getName().equals(name))
					return attribute;
		return null;
	}

	public void setAttributes(WSAttribute[] attributes) {
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
		List<WSAttribute> buf = new ArrayList<WSAttribute>();
		buf.addAll(Arrays.asList(getAttributes()));
		buf.add(att);
		setAttributes(buf.toArray(new WSAttribute[0]));
	}
}