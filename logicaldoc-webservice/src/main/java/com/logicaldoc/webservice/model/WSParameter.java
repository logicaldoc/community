package com.logicaldoc.webservice.model;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Useful class that contains a correspondence between a name and a value.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
@XmlType(name = "WSParameter")
public class WSParameter {

	private String name;

	private String value;
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return getName() + " - " + getValue();
	}
}
