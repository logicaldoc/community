package com.logicaldoc.util.config;

import org.jdom2.Element;

/**
 * Configurator class for the META-INF/context.properties
 * 
 * @author Marco Meschieri
 * @since 8.8.4
 */
public class WebContextConfigurator {

	protected XMLBean xml;

	public WebContextConfigurator(String resource) {
		if (getClass().getClassLoader().getResource(resource) != null)
			xml = new XMLBean(getClass().getClassLoader().getResource(resource));
		else
			xml = new XMLBean(resource);
	}

	public String getSameSiteCookies() {
		Element element = xml.getChild("CookieProcessor");
		return element.getAttributeValue("sameSiteCookies");
	}

	public boolean setSameSiteCookies(String sameSite) {
		if (!sameSite.equals(getSameSiteCookies())) {
			Element element = xml.getChild("CookieProcessor");
			element.setAttribute("sameSiteCookies", sameSite);
			xml.writeXMLDoc();
			return true;
		} else
			return false;

	}
}