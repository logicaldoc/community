package com.logicaldoc.util.config;

import java.util.HashMap;
import java.util.Map;

import org.jdom2.Element;

/**
 * Configurator class for spring's context-security setup.
 * 
 * @author Marco Meschieri
 * @since 8.7.3
 */
public class SecurityConfigurator {

	protected XMLBean xml;

	public SecurityConfigurator(String resource) {
		if (getClass().getClassLoader().getResource(resource) != null) {
			xml = new XMLBean(getClass().getClassLoader().getResource(resource));
		} else
			xml = new XMLBean(resource);
	}

	public SecurityConfigurator() {
		xml = new XMLBean(getClass().getClassLoader().getResource("context-security.xml"));
	}

	public String getContentSecurityPolicy() {
		Element header = getContentSecurityPolicyElement();
		if (header != null)
			return header.getAttributeValue("value");
		else
			return null;
	}

	public boolean setContentSecurityPolicy(String contentSecurityPolicy) {
		Element header = getContentSecurityPolicyElement();
		boolean modified = false;
		if (header != null) {
			String currentValue = header.getAttributeValue("value");
			if (currentValue != null && !currentValue.equals(contentSecurityPolicy)) {
				header.setAttribute("value", contentSecurityPolicy);
				modified = true;
			}
		}
		if (modified)
			xml.writeXMLDoc();
		return modified;
	}

	private Element getContentSecurityPolicyElement() {
		Map<String, String> namespaces=new HashMap<String, String>();
		namespaces.put("security", "http://www.springframework.org/schema/security");
		return xml.findElement("//security:header[@name='Content-Security-Policy']", namespaces);
	}
}