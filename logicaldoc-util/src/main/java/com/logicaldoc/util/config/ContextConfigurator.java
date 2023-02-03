package com.logicaldoc.util.config;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.jdom2.Attribute;
import org.jdom2.Element;

/**
 * Configurator class for spring's application context setup.
 * 
 * @author Marco Meschieri
 * @since 3.0
 */
public class ContextConfigurator {

	protected XMLBean xml;

	public ContextConfigurator(String resource) {
		if (getClass().getClassLoader().getResource(resource) != null)
			xml = new XMLBean(getClass().getClassLoader().getResource(resource));
		else
			xml = new XMLBean(resource);
	}

	public ContextConfigurator() {
		xml = new XMLBean(getClass().getClassLoader().getResource("context.xml"));
	}

	public void setProperty(String id, String propertyName, String value) {
		Element element = xml.getChild("bean", "id", id);
		for (Element property : element.getChildren("property", element.getNamespace())) {
			if (propertyName.equals(property.getAttribute("name").getValue())) {
				property.getChild("value").setText(value);
				break;
			}
		}
	}

	public void clearPropertyValue(String id, String propertyName) {
		Element element = xml.getChild("bean", "id", id);
		if (element == null)
			return;
		for (Element property : element.getChildren("property", element.getNamespace())) {
			if (propertyName.equals(property.getAttribute("name").getValue())) {
				property.removeContent();
				return;
			}
		}
	}

	public void addPropertyBeanRefList(String id, String propertyName, List<? extends String> values) {
		Element element = xml.getChild("bean", "id", id);
		if (element == null)
			return;

		for (Element property : element.getChildren("property", element.getNamespace())) {
			if (propertyName.equals(property.getAttribute("name").getValue())) {
				addPropertyListValues(property, values);
				break;
			}
		}
	}

	private void addPropertyListValues(Element property, List<? extends String> values) {
		Collection<Element> beanRefChildren = new LinkedList<>();
		Element listElement = property.getChild("list", property.getNamespace());
		if (listElement != null) {
			List<Element> elms = listElement.getChildren();
			for (Element elm : elms) {
				if (elm.getName().equals("ref"))
					beanRefChildren.add(elm);

			}
		} else {
			listElement = new Element("list");
		}

		for (String value : values) {
			Element refBeanElement = new Element("ref");
			refBeanElement.setAttribute(new Attribute("bean", value));
			beanRefChildren.add(refBeanElement);
		}
		listElement.removeContent();
		listElement.setContent(beanRefChildren);
		property.setContent(listElement);
	}

	public String getProperty(String id, String propertyName) {
		Element element = getPropertyElement(id, propertyName);
		if (element != null)
			return element.getChild("value").getText();
		return null;
	}

	/**
	 * Retrieves the prop value of the specified property, that is one inside
	 * the &lt;props&gt; tag:
	 * <p>
	 * 
	 * &lt;property&gt;&lt;props&gt;&lt;prop
	 * key="key_name"&gt;key_value&lt;/prop&gt;&lt;/props&gt;&lt;/property&gt;
	 * 
	 * @param id The bean id
	 * @param propertyName The property name
	 * @param key The pop key
	 * @return The prop value
	 */
	public String getPropertyProp(String id, String propertyName, String key) {
		Element element = getPropElement(id, propertyName, key);

		if (element != null)
			return element.getText();
		return null;
	}

	/**
	 * Retrieves the prop element of the specified property, that is one inside
	 * the &lt;props&gt; tag:
	 * <p>
	 * 
	 * &lt;property&gt;&lt;props&gt;&lt;prop
	 * key="key_name"&gt;key_value&lt;/prop&gt;&lt;/props&gt;&lt;/property&gt;
	 * 
	 * @param id The bean id
	 * @param propertyName The property name
	 * @param key The pop key
	 * @return The prop element
	 */
	protected Element getPropElement(String id, String propertyName, String key) {
		Element element = getPropertyElement(id, propertyName);
		Element props = element.getChild("props", element.getNamespace());
		for (Iterator iter = props.getChildren().iterator(); iter.hasNext();) {
			Element prop = (Element) iter.next();
			if (key.equals(prop.getAttributeValue("key")))
				return prop;
		}
		return null;
	}

	/**
	 * Sets the prop value of the specified property, that is one inside the
	 * &lt;props&gt; tag:
	 * <p>
	 * 
	 * &lt;property&gt;&lt;props&gt;&lt;prop
	 * key="key_name"&gt;key_value&lt;/prop&gt;&lt;/props&gt;&lt;/property&gt;
	 * 
	 * @param id The bean id
	 * @param propertyName The property name
	 * @param key The property key
	 * @param value The property value
	 */
	public void setPropValue(String id, String propertyName, String key, String value) {
		Element prop = getPropElement(id, propertyName, key);
		if (prop != null)
			prop.setText(value);
	}

	protected Element getPropertyElement(String id, String propertyName) {
		Element element = xml.getChild("bean", "id", id);
		if (element == null)
			return null;
		List properties = element.getChildren("property", element.getNamespace());
		for (Iterator iter = properties.iterator(); iter.hasNext();) {
			Element property = (Element) iter.next();
			Attribute nameAttribute = property.getAttribute("name");
			if (nameAttribute != null && propertyName.equals(nameAttribute.getValue())) {
				return property;
			}
		}
		return null;
	}

	public String getDialect() {
		return getPropertyProp("SessionFactory", "hibernateProperties", "hibernate.dialect");
	}

	public boolean write() {
		return xml.writeXMLDoc();
	}

	/**
	 * Enlists a new trigger in the scheduler
	 * 
	 * @param triggerId name of the trigger's bean
	 */
	public void addTrigger(String triggerId) {
		Element element = getPropertyElement("Scheduler", "triggers");
		Element list = element.getChild("list", element.getNamespace());
		List refs = list.getChildren("ref", element.getNamespace());
		for (Iterator iterator = refs.iterator(); iterator.hasNext();) {
			Element ref = (Element) iterator.next();
			if (triggerId.equals(ref.getAttribute("bean").getValue()))
				return;
		}

		Element ref = new Element("ref", element.getNamespace());
		ref.setAttribute("bean", triggerId);
		list.addContent(ref);

		xml.writeXMLDoc();
	}
}