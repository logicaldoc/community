package com.logicaldoc.util.config;

import java.util.Iterator;
import java.util.List;

import org.jdom2.Element;
import org.jdom2.Namespace;

/**
 * Configurator for the web.xml file
 * 
 * @author Marco Meschieri - LogicalDOC
 * @author Sebastian Wenzky
 * @since 3.0
 */
public class WebConfigurator extends XMLBean {

	public static enum INIT_PARAM {
		PARAM_OVERWRITE, PARAM_APPEND, PARAM_STOP
	}

	public WebConfigurator(String path) {
		super(path);
	}

	/**
	 * Check for existing element within a XML-document
	 * 
	 * @param elements List of Elements which have as child a
	 *        <pre>
	 *        name
	 *        </pre>
	 *        tag
	 * @param match_text The text for looking up whether exists
	 * @param name The tag that should be right there for checking this value
	 * 
	 * @return the element
	 */
	private Element elementLookUp(List elements, String match_text, String name) {
		for (Iterator iterator = elements.iterator(); iterator.hasNext();) {
			Element elem = (Element) iterator.next();
			Element elementName = elem.getChild(match_text, elem.getNamespace());
			if (elementName != null && elementName.getText().trim().equals(name)) {
				// The element already exists
				return elem;
			}
		}

		return null;
	}

	/**
	 * Adding a contextparam to the web.xml
	 * 
	 * @param name the param
	 * @param value the value
	 * @param description description of the parameter
	 * @param append * @param append if the param exist, should the new value
	 *        appended? possible values are represented in
	 *        {@link WebConfigurator.INIT_PARAM}
	 */
	public void addContextParam(String name, String value, String description, INIT_PARAM append) {
		List contextParams = getRootElement().getChildren("context-param", getRootElement().getNamespace());
		Element contextParam = this.elementLookUp(contextParams, "param-name", name);

		if (contextParam != null && append.equals(INIT_PARAM.PARAM_STOP))
			return;

		if (contextParam == null) {
			// Retrieve the last <servlet> element
			Element lastContextParam = (Element) contextParams.get(contextParams.size() - 1);

			List children = getRootElement().getChildren();

			// Find the index of the element to add the new element after.
			int index = children.indexOf(contextParams);

			// Prepare the new mapping
			contextParam = new Element("context-param", getRootElement().getNamespace());
			Element paramName = new Element("param-name", getRootElement().getNamespace());
			paramName.setText(name);
			Element paramValue = new Element("param-value", getRootElement().getNamespace());
			paramValue.setText(value);
			contextParam.addContent("\n ");
			contextParam.addContent(paramName);
			contextParam.addContent("\n ");
			contextParam.addContent(paramValue);
			contextParam.addContent("\n ");

			// Add the new element to the next index along.
			// This does cover the case where indexOf returned -1.
			children.add(index + 1, contextParam);
			writeXMLDoc();

			return;
		}

		if (contextParam != null && append.equals(INIT_PARAM.PARAM_APPEND)) {

			Element paramValue = (Element) contextParam.getChildren().get(1);
			paramValue.setText(paramValue.getText() + "," + value);
			writeXMLDoc();
			return;
		}

		if (contextParam != null && append.equals(INIT_PARAM.PARAM_OVERWRITE)) {
			Element paramValue = (Element) contextParam.getChildren().get(1);
			paramValue.setText(value);
			writeXMLDoc();
			return;
		}
	}

	public void addFilterInitParam(String filterName, String param_name, String param_value) {
		this.addFilterInitParam(filterName, param_name, param_value, null, INIT_PARAM.PARAM_STOP);
	}

	/**
	 * Adds a init parameter to the filter
	 * 
	 * @param filterName The name of the filter
	 * @param name Name of the Parameter
	 * @param value Value of the Parameter
	 * @param description Description
	 * @param append if the parameter exist, should the new value appended?
	 *        possible values are represented in
	 *        {@link WebConfigurator.INIT_PARAM}
	 */
	public void addFilterInitParam(String filterName, String name, String value, String description,
			INIT_PARAM append) {
		List filters = getRootElement().getChildren("filter", getRootElement().getNamespace());
		Element filter = this.elementLookUp(filters, "filter-name", filterName);

		if (filter == null)
			throw new IllegalStateException(
					"The filter " + filterName + " has not been found. Have you already written the filter?");

		Element initParam = this.elementLookUp(filter.getChildren(), "param-name", name);

		if (initParam != null && append.equals(INIT_PARAM.PARAM_STOP))
			return;

		if (initParam != null && append.equals(INIT_PARAM.PARAM_APPEND)) {
			Element paramValue = ((Element) initParam.getParent()).getChild("param-value");
			paramValue.setText(paramValue.getText() + "," + value);
			writeXMLDoc();
			return;
		}

		if (initParam != null && append.equals(INIT_PARAM.PARAM_OVERWRITE)) {
			Element paramValue = ((Element) initParam.getParent()).getChild("param-value");
			paramValue.setText(value);
			writeXMLDoc();
			return;
		}

		Element paramElement = new Element("init-param", getRootElement().getNamespace());

		// the name
		Element param = new Element("param-name", getRootElement().getNamespace());
		param.setText(name);
		// paramElement.addContent("\n ");
		paramElement.getChildren().add(param);

		param = new Element("param-value", getRootElement().getNamespace());
		param.setText(value);
		// paramElement.addContent("\n ");
		paramElement.getChildren().add(param);

		if (description != null && description.equals("") != true) {
			param = new Element("description", getRootElement().getNamespace());
			param.setText(description);
			// paramElement.addContent("\n ");
			paramElement.getChildren().add(param);
		}

		filter.getChildren().add(paramElement);

	}

	/**
	 * Adds a init parameter to the servlet
	 * 
	 * @param servletName The name of the servlet
	 * @param name Name of the Parameter
	 * @param value Value of the Parameter
	 * @param description Description
	 * @param append if the param exist, should the new value appended? possible
	 *        values are represented in {@link WebConfigurator.INIT_PARAM}
	 */
	public void addInitParam(String servletName, String name, String value, String description,
			INIT_PARAM append) {
		List servlets = getRootElement().getChildren("servlet", getRootElement().getNamespace());
		Element servlet = this.elementLookUp(servlets, "servlet-name", servletName);

		if (servlet == null)
			throw new IllegalStateException(
					"The servlet " + servletName + " has not been found. Have you already written the servlet?");

		Element initParam = this.elementLookUp(servlet.getChildren(), "param-name", name);

		if (initParam != null && append.equals(INIT_PARAM.PARAM_STOP))
			return;

		if (initParam != null && append.equals(INIT_PARAM.PARAM_APPEND)) {
			Element paramValue = ((Element) initParam.getParent()).getChild("param-value");
			paramValue.setText(paramValue.getText() + "," + value);
			writeXMLDoc();
			return;
		}

		if (initParam != null && append.equals(INIT_PARAM.PARAM_OVERWRITE)) {
			Element paramValue = ((Element) initParam.getParent()).getChild("param-value");
			paramValue.setText(value);
			writeXMLDoc();
			return;
		}

		Element paramElement = new Element("init-param", getRootElement().getNamespace());

		// the name
		Element param = new Element("param-name", getRootElement().getNamespace());
		param.setText(name);
		// paramElement.addContent("\n ");
		paramElement.getChildren().add(param);

		param = new Element("param-value", getRootElement().getNamespace());
		param.setText(value);
		// paramElement.addContent("\n ");
		paramElement.getChildren().add(param);

		if (description != null && description.equals("") != true) {
			param = new Element("description", getRootElement().getNamespace());
			param.setText(description);
			// paramElement.addContent("\n ");
			paramElement.getChildren().add(param);
		}

		Element loadOnStartUpElem = (Element) servlet.getChildren().get(servlet.getChildren().size() - 1);

		// sorting the elements in that way, that element "load-on-startup"
		// always stays at the tail
		if (loadOnStartUpElem.getName().equals("load-on-startup")) {
			servlet.getChildren().remove(loadOnStartUpElem);
			servlet.addContent(paramElement);
			servlet.addContent(loadOnStartUpElem);
		} else
			servlet.getChildren().add(paramElement);

	}

	/**
	 * Adds a init parameter to the listener
	 * 
	 * @param listenerClazz The class of the listener
	 * @param name Name of the Parameter
	 * @param value Value of the Parameter
	 * @param append if the param exist, should the new value appended? possible
	 *        values are represented in {@link WebConfigurator.INIT_PARAM}
	 */
	public void addListenerInitParam(String listenerClazz, String name, String value, INIT_PARAM append) {
		List listeners = getRootElement().getChildren("listener", getRootElement().getNamespace());
		Element listener = this.elementLookUp(listeners, "listener-class", listenerClazz);

		if (listener == null)
			throw new IllegalStateException(
					"The listener " + listenerClazz + " has not been found. Have you already written the listener?");

		Element initParam = this.elementLookUp(listener.getChildren(), "param-name", name);

		if (initParam != null && append.equals(INIT_PARAM.PARAM_STOP))
			return;

		if (initParam != null && append.equals(INIT_PARAM.PARAM_APPEND)) {
			Element paramValue = ((Element) initParam.getParent()).getChild("param-value");
			paramValue.setText(paramValue.getText() + "," + value);
			writeXMLDoc();
			return;
		}

		if (initParam != null && append.equals(INIT_PARAM.PARAM_OVERWRITE)) {
			Element paramValue = ((Element) initParam.getParent()).getChild("param-value");
			paramValue.setText(value);
			writeXMLDoc();
			return;
		}

		Element paramElement = new Element("init-param", getRootElement().getNamespace());

		// the name
		Element param = new Element("param-name", getRootElement().getNamespace());
		param.setText(name);
		// paramElement.addContent("\n ");
		paramElement.getChildren().add(param);

		param = new Element("param-value", getRootElement().getNamespace());
		param.setText(value);
		// paramElement.addContent("\n ");
		paramElement.getChildren().add(param);
		listener.getChildren().add(paramElement);
	}

	/**
	 * Adds a init parameter to the servlet
	 * 
	 * @param servletName name of the servlet
	 * @param name Name of the Parameter
	 * @param value Value of the Parameter
	 * @param description Description
	 */
	public void addInitParam(String servletName, String name, String value, String description) {
		this.addInitParam(servletName, name, value, description, INIT_PARAM.PARAM_STOP);
	}

	/**
	 * Adds a new servlet mapping to the deployment descriptor. If the mapping
	 * already exists no modifications are committed.
	 * 
	 * @param name The servlet name
	 * @param clazz The servlet class fully qualified name
	 */
	public void addServlet(String name, String clazz) {
		this.addServlet(name, clazz, -1);
	}

	/**
	 * Adds a new servlet to the deployment descriptor. If the servlet already
	 * exists no modifications are committed.
	 * 
	 * @param name The servlet name
	 * @param clazz The servlet class fully qualified name
	 * @param loadOnStartup use 1 in case the servlet must be started during the startup
	 */
	@SuppressWarnings("unchecked")
	public void addServlet(String name, String clazz, int loadOnStartup) {
		Element servlet = null;
		Element servletClass = null;

		// Search for the specified servlet
		List servlets = getRootElement().getChildren("servlet", getRootElement().getNamespace());
		servlet = this.elementLookUp(servlets, "servlet-name", name);
		if (servlet != null) {
			// The servlet already exists, so update it
			servletClass = servlet.getChild("servlet-class", getRootElement().getNamespace());
			servletClass.setText(clazz);
		} else {

			// Retrieve the last <servlet> element
			Element lastServlet = (Element) servlets.get(servlets.size() - 1);

			List children = getRootElement().getChildren();

			// Find the index of the element to add the new element after.
			int index = children.indexOf(lastServlet);

			// Prepare the new mapping
			servlet = new Element("servlet", getRootElement().getNamespace());
			Element servletNameElement = new Element("servlet-name", getRootElement().getNamespace());
			servletNameElement.setText(name);
			servletClass = new Element("servlet-class", getRootElement().getNamespace());
			servletClass.setText(clazz);
			servlet.addContent("\n ");
			servlet.addContent(servletNameElement);
			servlet.addContent("\n ");
			servlet.addContent(servletClass);
			servlet.addContent("\n ");

			// Add the new element to the next index along.
			// This does cover the case where indexOf returned -1.
			children.add(index + 1, servlet);
		}

		writeXMLDoc();
	}

	/**
	 * Adds a new filter to the deployment descriptor. If the filter already
	 * exists no modifications are committed.
	 * 
	 * @param name The filter name
	 * @param clazz The filter class fully qualified name
	 */
	@SuppressWarnings("unchecked")
	public void addFilter(String name, String clazz) {
		Element filter = null;
		Element filterClass = null;

		// Search for the specified filter
		List filters = getRootElement().getChildren("filter", getRootElement().getNamespace());
		filter = this.elementLookUp(filters, "filter-name", name);
		if (filter != null) {
			// The filter already exists, so update it
			filterClass = filter.getChild("filter-class", getRootElement().getNamespace());
			filterClass.setText(clazz);
		} else {

			// Retrieve the last <filter> element
			Element lastFilter = (Element) filters.get(filters.size() - 1);

			List children = getRootElement().getChildren();

			// Find the index of the element to add the new element after.
			int index = children.indexOf(lastFilter);

			// Prepare the new mapping
			filter = new Element("filter", getRootElement().getNamespace());
			Element filterNameElement = new Element("filter-name", getRootElement().getNamespace());
			filterNameElement.setText(name);
			filterClass = new Element("filter-class", getRootElement().getNamespace());
			filterClass.setText(clazz);
			filter.addContent("\n ");
			filter.addContent(filterNameElement);
			filter.addContent("\n ");
			filter.addContent(filterClass);
			filter.addContent("\n ");

			// Add the new element to the next index along.
			// This does cover the case where indexOf returned -1.
			children.add(index + 1, filter);
		}

		writeXMLDoc();
	}

	/**
	 * Adds a new listener to the deployment descriptor. If the listener already
	 * exists no modifications are committed. e
	 * 
	 * @param clazz The filter class fully qualified name
	 */
	@SuppressWarnings("unchecked")
	public void addListener(String clazz) {
		Element listener = null;
		Element listenerClass = null;

		// Search for the specified filter
		List listeners = getRootElement().getChildren("listener", getRootElement().getNamespace());
		listener = this.elementLookUp(listeners, "listener-class", clazz);
		if (listener == null) {
			// The listener doesn't exist, so create it
			// Retrieve the last <listener> element
			Element lastListener = (Element) listeners.get(listeners.size() - 1);

			List children = getRootElement().getChildren();

			// Find the index of the element to add the new element after.
			int index = children.indexOf(lastListener);

			// Prepare the new mapping
			listener = new Element("listener", getRootElement().getNamespace());
			listenerClass = new Element("listener-class", getRootElement().getNamespace());
			listenerClass.setText(clazz);
			listener.addContent("\n ");
			listener.addContent(listenerClass);
			listener.addContent("\n ");

			// Add the new element to the next index along.
			// This does cover the case where indexOf returned -1.
			children.add(index + 1, listener);
		}

		writeXMLDoc();
	}

	/**
	 * Adds a new servlet mapping to the deployment descriptor. If the mapping
	 * already exists no modifications are committed.
	 * 
	 * @param servlet The name of the servlet
	 * @param pattern The mapping pattern
	 */
	@SuppressWarnings("unchecked")
	public void addServletMapping(String servlet, String pattern) {
		// Search for the specified mapping
		List mappings = getRootElement().getChildren("servlet-mapping", getRootElement().getNamespace());
		for (Iterator iterator = mappings.iterator(); iterator.hasNext();) {
			Element elem = (Element) iterator.next();
			Element servletName = elem.getChild("servlet-name", elem.getNamespace());
			Element urlPattern = elem.getChild("url-pattern", elem.getNamespace());

			if (servletName.getText().trim().equals(servlet) && urlPattern.getText().trim().equals(pattern)) {
				// The mapping already exists
				return;
			}
		}

		// Retrieve the last <servlet-mapping> element
		Element lastMapping = (Element) mappings.get(mappings.size() - 1);

		List children = getRootElement().getChildren();
		// Find the index of the element to add the new element after.
		int index = children.indexOf(lastMapping);

		// Prepare the new mapping
		Element servletMapping = new Element("servlet-mapping", getRootElement().getNamespace());
		Element servletName = new Element("servlet-name", getRootElement().getNamespace());
		servletName.setText(servlet);
		Element servletPattern = new Element("url-pattern", getRootElement().getNamespace());
		servletPattern.setText(pattern);
		servletMapping.addContent("\n ");
		servletMapping.addContent(servletName);
		servletMapping.addContent("\n ");
		servletMapping.addContent(servletPattern);
		servletMapping.addContent("\n ");

		// Add the new element to the next index along.
		// This does cover the case where indexOf returned -1.
		children.add(index + 1, servletMapping);
		writeXMLDoc();
	}

	/**
	 * Adds a new filter mapping to the deployment descriptor. If the mapping
	 * already exists no modifications are committed.
	 * 
	 * @param filter The name of the filter
	 * @param pattern The mapping pattern
	 */
	@SuppressWarnings("unchecked")
	public void addFilterMapping(String filter, String pattern) {
		// Search for the specified mapping
		List mappings = getRootElement().getChildren("filter-mapping", getRootElement().getNamespace());
		for (Iterator iterator = mappings.iterator(); iterator.hasNext();) {
			Element elem = (Element) iterator.next();
			Element filterName = elem.getChild("filter-name", elem.getNamespace());
			Element urlPattern = elem.getChild("url-pattern", elem.getNamespace());

			if (filterName.getText().trim().equals(filter) && urlPattern.getText().trim().equals(pattern)) {
				// The mapping already exists
				return;
			}
		}

		// Retrieve the last <servlet-mapping> element
		Element lastMapping = (Element) mappings.get(mappings.size() - 1);

		List children = getRootElement().getChildren();
		// Find the index of the element to add the new element after.
		int index = children.indexOf(lastMapping);

		// Prepare the new mapping
		Element filterMapping = new Element("filter-mapping", getRootElement().getNamespace());
		Element filterName = new Element("filter-name", getRootElement().getNamespace());
		filterName.setText(filter);
		Element filterPattern = new Element("url-pattern", getRootElement().getNamespace());
		filterPattern.setText(pattern);
		filterMapping.addContent("\n ");
		filterMapping.addContent(filterName);
		filterMapping.addContent("\n ");
		filterMapping.addContent(filterPattern);
		filterMapping.addContent("\n ");

		// Add the new element to the next index along.
		// This does cover the case where indexOf returned -1.
		children.add(index + 1, filterMapping);
		writeXMLDoc();
	}

	public void setDisplayName(String displayName) {
		// Retrieve the <display-name> element
		Element element = getRootElement().getChild("display-name", getRootElement().getNamespace());
		element.setText(displayName);
		writeXMLDoc();
	}

	public String getDisplayName() {
		// Retrieve the <display-name> element
		Element element = getRootElement().getChild("display-name", getRootElement().getNamespace());
		return element.getText();
	}

	public void setDescription(String description) {
		// Retrieve the <display-name> element
		Element element = getRootElement().getChild("description", getRootElement().getNamespace());
		element.setText(description);
		writeXMLDoc();
	}

	/**
	 * Sets the given policy in the &lt;transport-guarantee&gt; tags
	 *
	 * @param policy the policy to set
	 * 
	 * @return true if the XML was touched
	 */
	@SuppressWarnings("rawtypes")
	public boolean setTransportGuarantee(String policy) {
		boolean modified = false;
		Namespace namespace = getRootElement().getNamespace();
		List constraints = getRootElement().getChildren("security-constraint", namespace);
		for (Iterator iterator = constraints.iterator(); iterator.hasNext();) {
			Element constraint = (Element) iterator.next();
			if (constraint == null)
				continue;
			Element userData = constraint.getChild("user-data-constraint", namespace);
			if (userData == null)
				continue;
			Element transport = userData.getChild("transport-guarantee", namespace);
			if (transport == null)
				continue;
			if (!transport.getText().equals(policy)) {
				transport.setText(policy);
				modified = true;
			}
		}

		if (modified)
			writeXMLDoc();

		return modified;
	}
}