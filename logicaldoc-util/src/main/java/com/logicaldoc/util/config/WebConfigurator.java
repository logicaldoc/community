package com.logicaldoc.util.config;

import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
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

    private static final String S_S = "%s,%s";

    private static final String INIT_PARAM_STR = "init-param";

    private static final String URL_PATTERN = "url-pattern";

    private static final String LISTENER_CLASS = "listener-class";

    private static final String LISTENER = "listener";

    private static final String SERVLET_NAME = "servlet-name";

    private static final String SERVLET = "servlet";

    private static final String DESCRIPTION = "description";

    private static final String FILTER_NAME = "filter-name";

    private static final String FILTER = "filter";

    private static final String PARAM_VALUE = "param-value";

    private static final String PARAM_NAME = "param-name";

    public enum ParamInsertMode {
        OVERWRITE, APPEND, STOP
    }

    public WebConfigurator(String path) {
        super(path);
    }

    /**
     * Check for existing element within a XML-document
     * 
     * @param elements List of Elements which have as child a
     * 
     *        <pre>
     *        name
     *        </pre>
     * 
     *        tag
     * @param matchText The text for looking up whether exists
     * @param name The tag that should be right there for checking this value
     * 
     * @return the element
     */
    private Element elementLookUp(List<Element> elements, String matchText, String name) {
        for (Iterator<Element> iterator = elements.iterator(); iterator.hasNext();) {
            Element elem = iterator.next();
            Element elementName = elem.getChild(matchText, elem.getNamespace());
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
     * @param appendMode if the param exist, should the new value appended?
     *        possible values are represented in
     *        {@link WebConfigurator.ParamInsertMode}
     */
    public void addContextParam(String name, String value, String description, ParamInsertMode appendMode) {
        List<Element> contextParams = getRootElement().getChildren("context-param", rootNamespace());
        Element contextParam = this.elementLookUp(contextParams, PARAM_NAME, name);

        if (contextParam != null && appendMode.equals(ParamInsertMode.STOP))
            return;

        if (contextParam == null) {
            // Retrieve the last <servlet> element
            Element lastContextParam = contextParams.get(contextParams.size() - 1);

            List<Element> children = getRootElement().getChildren();

            // Find the index of the element to add the new element after.
            int index = children.indexOf(lastContextParam);

            // Prepare the new mapping
            contextParam = new Element("context-param", rootNamespace());
            Element paramName = new Element(PARAM_NAME, rootNamespace());
            paramName.setText(name);
            Element paramValue = new Element(PARAM_VALUE, rootNamespace());
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

        if (appendMode.equals(ParamInsertMode.APPEND)) {
            Element paramValue = contextParam.getChildren().get(1);
            paramValue.setText(S_S.formatted(paramValue.getText(), value));
            writeXMLDoc();
            return;
        }

        if (appendMode.equals(ParamInsertMode.OVERWRITE)) {
            Element paramValue = contextParam.getChildren().get(1);
            paramValue.setText(value);
        }

        writeXMLDoc();
    }

    public void addFilterInitParam(String filterName, String paramName, String paramValue) {
        this.addFilterInitParam(filterName, paramName, paramValue, null, ParamInsertMode.STOP);
    }

    /**
     * Adds a init parameter to the filter
     * 
     * @param filterName The name of the filter
     * @param paramName Name of the Parameter
     * @param value Value of the Parameter
     * @param description Description
     * @param insertMode if the parameter exist, should the new value appended?
     *        possible values are represented in
     *        {@link WebConfigurator.ParamInsertMode}
     */
    public void addFilterInitParam(
            String filterName,
            String paramName,
            String value,
            String description,
            ParamInsertMode insertMode) {
        List<Element> filters = getRootElement().getChildren(FILTER, rootNamespace());
        Element filter = this.elementLookUp(filters, FILTER_NAME, filterName);

        if (filter == null)
            throw new IllegalStateException(
                    "The filter %s has not been found. Have you already written the filter?".formatted(filterName));

        Element initParam = this.elementLookUp(filter.getChildren(), PARAM_NAME, paramName);

        if (initParam != null && insertMode.equals(ParamInsertMode.STOP))
            return;

        if (initParam != null
                && (insertMode.equals(ParamInsertMode.APPEND) || insertMode.equals(ParamInsertMode.OVERWRITE))) {
            Element paramValue = initParam.getChild(PARAM_VALUE, rootNamespace());
            if (paramValue == null) {
                paramValue = new Element(PARAM_VALUE, rootNamespace());
                initParam.getChildren().add(paramValue);
            }

            if (insertMode.equals(ParamInsertMode.OVERWRITE))
                paramValue.setText(value);
            else
                paramValue.setText(S_S.formatted(paramValue.getText(), value));

            writeXMLDoc();
            return;
        }

        Element paramElement = new Element(INIT_PARAM_STR, rootNamespace());

        // the name
        Element param = new Element(PARAM_NAME, rootNamespace());
        param.setText(paramName);
        paramElement.getChildren().add(param);

        param = new Element(PARAM_VALUE, rootNamespace());
        param.setText(value);
        paramElement.getChildren().add(param);

        if (StringUtils.isNotEmpty(description)) {
            param = new Element(DESCRIPTION, rootNamespace());
            param.setText(description);
            paramElement.getChildren().add(param);
        }

        filter.getChildren().add(paramElement);
        writeXMLDoc();
    }

    /**
     * Adds a init parameter to the servlet
     * 
     * @param servletName The name of the servlet
     * @param paramName Name of the Parameter
     * @param value Value of the Parameter
     * @param description Description
     * @param insertMode if the param exist, should the new value appended?
     *        possible values are represented in
     *        {@link WebConfigurator.ParamInsertMode}
     */
    public void addServletInitParam(
            String servletName,
            String paramName,
            String value,
            String description,
            ParamInsertMode insertMode) {
        List<Element> servlets = getRootElement().getChildren(SERVLET, rootNamespace());
        Element servlet = this.elementLookUp(servlets, SERVLET_NAME, servletName);

        if (servlet == null)
            throw new IllegalStateException(
                    "The servlet %s has not been found. Have you already written the servlet?".formatted(servletName));

        Element initParam = this.elementLookUp(servlet.getChildren(), PARAM_NAME, paramName);

        if (initParam != null && insertMode.equals(ParamInsertMode.STOP))
            return;

        if (initParam != null
                && (insertMode.equals(ParamInsertMode.APPEND) || insertMode.equals(ParamInsertMode.OVERWRITE))) {
            Element paramValue = initParam.getChild(PARAM_VALUE, rootNamespace());
            if (paramValue == null) {
                paramValue = new Element(PARAM_VALUE, rootNamespace());
                initParam.getChildren().add(paramValue);
            }

            if (insertMode.equals(ParamInsertMode.OVERWRITE))
                paramValue.setText(value);
            else
                paramValue.setText(S_S.formatted(paramValue.getText(), value));

            writeXMLDoc();
            return;
        }

        Element paramElement = new Element(INIT_PARAM_STR, rootNamespace());

        // the name
        Element param = new Element(PARAM_NAME, rootNamespace());
        param.setText(paramName);
        paramElement.getChildren().add(param);

        param = new Element(PARAM_VALUE, rootNamespace());
        param.setText(value);
        paramElement.getChildren().add(param);

        if (StringUtils.isNotEmpty(description)) {
            param = new Element(DESCRIPTION, rootNamespace());
            param.setText(description);
            paramElement.getChildren().add(param);
        }

        Element loadOnStartUpElem = servlet.getChildren().get(servlet.getChildren().size() - 1);

        // sorting the elements in that way, that element "load-on-startup"
        // always stays at the tail
        if (loadOnStartUpElem.getName().equals("load-on-startup")) {
            servlet.getChildren().remove(loadOnStartUpElem);
            servlet.addContent(paramElement);
            servlet.addContent(loadOnStartUpElem);
        } else
            servlet.getChildren().add(paramElement);

        writeXMLDoc();
    }

    /**
     * Adds a init parameter to the servlet
     * 
     * @param servletName name of the servlet
     * @param paramName Name of the Parameter
     * @param value Value of the Parameter
     * @param description Description
     */
    public void addServletInitParam(String servletName, String paramName, String value, String description) {
        this.addServletInitParam(servletName, paramName, value, description, ParamInsertMode.STOP);
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
     * @param loadOnStartup use 1 in case the servlet must be started during the
     *        startup
     */
    public void addServlet(String name, String clazz, int loadOnStartup) {
        Element servlet = null;
        Element servletClass = null;

        // Search for the specified servlet
        List<Element> servlets = getRootElement().getChildren(SERVLET, rootNamespace());
        servlet = this.elementLookUp(servlets, SERVLET_NAME, name);
        if (servlet != null) {
            // The servlet already exists, so update it
            servletClass = servlet.getChild("servlet-class", rootNamespace());
            servletClass.setText(clazz);
        } else {

            // Retrieve the last <servlet> element
            Element lastServlet = servlets.get(servlets.size() - 1);

            List<Element> children = getRootElement().getChildren();

            // Find the index of the element to add the new element after.
            int index = children.indexOf(lastServlet);

            // Prepare the new mapping
            servlet = new Element(SERVLET, rootNamespace());
            Element servletNameElement = new Element(SERVLET_NAME, rootNamespace());
            servletNameElement.setText(name);
            servletClass = new Element("servlet-class", rootNamespace());
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
    public void addFilter(String name, String clazz) {
        Element filter = null;
        Element filterClass = null;

        // Search for the specified filter
        List<Element> filters = getRootElement().getChildren(FILTER, rootNamespace());
        filter = this.elementLookUp(filters, FILTER_NAME, name);
        if (filter != null) {
            // The filter already exists, so update it
            filterClass = filter.getChild("filter-class", rootNamespace());
            filterClass.setText(clazz);
        } else {

            // Retrieve the last <filter> element
            Element lastFilter = filters.get(filters.size() - 1);

            List<Element> children = getRootElement().getChildren();

            // Find the index of the element to add the new element after.
            int index = children.indexOf(lastFilter);

            // Prepare the new mapping
            filter = new Element(FILTER, rootNamespace());
            Element filterNameElement = new Element(FILTER_NAME, rootNamespace());
            filterNameElement.setText(name);
            filterClass = new Element("filter-class", rootNamespace());
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
    public void addListener(String clazz) {
        Element listener = null;
        Element listenerClass = null;

        // Search for the specified filter
        List<Element> listeners = getRootElement().getChildren(LISTENER, rootNamespace());
        listener = this.elementLookUp(listeners, LISTENER_CLASS, clazz);
        if (listener == null) {
            // The listener doesn't exist, so create it
            // Retrieve the last <listener> element
            Element lastListener = listeners.get(listeners.size() - 1);

            List<Element> children = getRootElement().getChildren();

            // Find the index of the element to add the new element after.
            int index = children.indexOf(lastListener);

            // Prepare the new mapping
            listener = new Element(LISTENER, rootNamespace());
            listenerClass = new Element(LISTENER_CLASS, rootNamespace());
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
     * Adds a init parameter to the listener
     * 
     * @param listenerClazz The class of the listener
     * @param paramName Name of the Parameter
     * @param value Value of the Parameter
     * @param append if the param exist, should the new value appended? possible
     *        values are represented in {@link WebConfigurator.ParamInsertMode}
     */
    public void addListenerInitParam(String listenerClazz, String paramName, String value, ParamInsertMode append) {
        List<Element> listeners = getRootElement().getChildren(LISTENER, rootNamespace());
        Element listener = this.elementLookUp(listeners, LISTENER_CLASS, listenerClazz);

        if (listener == null)
            throw new IllegalStateException("The listener %s has not been found. Have you already written the listener?"
                    .formatted(listenerClazz));

        Element initParam = this.elementLookUp(listener.getChildren(), PARAM_NAME, paramName);

        if (initParam != null && append.equals(ParamInsertMode.STOP))
            return;

        if (initParam != null && append.equals(ParamInsertMode.APPEND)) {
            Element paramValue = initParam.getChild(PARAM_VALUE, rootNamespace());
            paramValue.setText(S_S.formatted(paramValue.getText(), value));
            writeXMLDoc();
            return;
        }

        if (initParam != null && append.equals(ParamInsertMode.OVERWRITE)) {
            Element paramValue = initParam.getChild(PARAM_VALUE, rootNamespace());
            paramValue.setText(value);
            writeXMLDoc();
            return;
        }

        Element paramElement = new Element(INIT_PARAM_STR, rootNamespace());

        // the name
        Element param = new Element(PARAM_NAME, rootNamespace());
        param.setText(paramName);
        paramElement.getChildren().add(param);

        param = new Element(PARAM_VALUE, rootNamespace());
        param.setText(value);
        paramElement.getChildren().add(param);
        listener.getChildren().add(paramElement);
        writeXMLDoc();
    }

    /**
     * Adds a new servlet mapping to the deployment descriptor. If the mapping
     * already exists no modifications are committed.
     * 
     * @param servlet The name of the servlet
     * @param pattern The mapping pattern
     */
    public void addServletMapping(String servlet, String pattern) {
        // Search for the specified mapping
        List<Element> mappings = getRootElement().getChildren("servlet-mapping", rootNamespace());
        for (Iterator<Element> iterator = mappings.iterator(); iterator.hasNext();) {
            Element elem = iterator.next();
            Element servletName = elem.getChild(SERVLET_NAME, elem.getNamespace());
            Element urlPattern = elem.getChild(URL_PATTERN, elem.getNamespace());

            if (servletName.getText().trim().equals(servlet) && urlPattern.getText().trim().equals(pattern)) {
                // The mapping already exists
                return;
            }
        }

        // Retrieve the last <servlet-mapping> element
        Element lastMapping = mappings.get(mappings.size() - 1);

        List<Element> children = getRootElement().getChildren();
        // Find the index of the element to add the new element after.
        int index = children.indexOf(lastMapping);

        // Prepare the new mapping
        Element servletMapping = new Element("servlet-mapping", rootNamespace());
        Element servletName = new Element(SERVLET_NAME, rootNamespace());
        servletName.setText(servlet);
        Element servletPattern = new Element(URL_PATTERN, rootNamespace());
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
    public void addFilterMapping(String filter, String pattern) {
        // Search for the specified mapping
        List<Element> mappings = getRootElement().getChildren("filter-mapping", rootNamespace());
        for (Iterator<Element> iterator = mappings.iterator(); iterator.hasNext();) {
            Element elem = iterator.next();
            Element filterName = elem.getChild(FILTER_NAME, elem.getNamespace());
            Element urlPattern = elem.getChild(URL_PATTERN, elem.getNamespace());

            if (filterName.getText().trim().equals(filter) && urlPattern.getText().trim().equals(pattern)) {
                // The mapping already exists
                return;
            }
        }

        // Retrieve the last <servlet-mapping> element
        Element lastMapping = mappings.get(mappings.size() - 1);

        List<Element> children = getRootElement().getChildren();
        // Find the index of the element to add the new element after.
        int index = children.indexOf(lastMapping);

        // Prepare the new mapping
        Element filterMapping = new Element("filter-mapping", rootNamespace());
        Element filterName = new Element(FILTER_NAME, rootNamespace());
        filterName.setText(filter);
        Element filterPattern = new Element(URL_PATTERN, rootNamespace());
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
        Element element = getRootElement().getChild("display-name", rootNamespace());
        element.setText(displayName);
        writeXMLDoc();
    }

    public String getDisplayName() {
        // Retrieve the <display-name> element
        Element element = getRootElement().getChild("display-name", rootNamespace());
        return element.getText();
    }

    public void setDescription(String description) {
        // Retrieve the <display-name> element
        Element element = getRootElement().getChild(DESCRIPTION, rootNamespace());
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
        Namespace namespace = rootNamespace();
        List constraints = getRootElement().getChildren("security-constraint", namespace);
        for (Iterator iterator = constraints.iterator(); iterator.hasNext();) {
            Element constraint = (Element) iterator.next();
            Element userData = constraint.getChild("user-data-constraint", namespace);
            if (userData != null) {
                Element transport = userData.getChild("transport-guarantee", namespace);
                if (transport != null && !transport.getText().equals(policy)) {
                    transport.setText(policy);
                    modified = true;
                }
            }
        }

        if (modified)
            writeXMLDoc();

        return modified;
    }
}