package com.logicaldoc.util.config;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLDecoder;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.xml.XMLConstants;

import org.apache.commons.io.FileUtils;
import org.jdom2.Attribute;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.Namespace;
import org.jdom2.filter.Filters;
import org.jdom2.input.SAXBuilder;
import org.jdom2.input.sax.XMLReaders;
import org.jdom2.output.Format;
import org.jdom2.output.XMLOutputter;
import org.jdom2.xpath.XPathExpression;
import org.jdom2.xpath.XPathFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class for using xml-files.
 * 
 * @author Michael Scholz
 * @version 1.0
 */
public class XMLBean {

	protected static Logger log = LoggerFactory.getLogger(XMLBean.class);

	private Document doc;

	private Element root;

	/** this points to an ordinary file */
	private String docPath;

	/** this points to an input stream; it is read-only! */
	private InputStream docInputStream = null;

	/**
	 * Creates new XMLBean.
	 * 
	 * @param docPath Path of xml-file.
	 */
	public XMLBean(String docPath) {
		this.docPath = docPath;
		docInputStream = null;
		initDocument();
	}

	/**
	 * Creates new XMLBean.
	 * 
	 * @param docname URL of xml-file.
	 */
	public XMLBean(URL docname) {

		try {
			docPath = URLDecoder.decode(docname.getPath(), "UTF-8");
			docInputStream = null;
		} catch (Exception ex) {
			log.error(ex.getMessage());
		}

		initDocument();
	}

	/**
	 * Creates new XMLBean from an input stream; XMLBean is read-only!!!
	 * 
	 * @param is contents of the XML structure
	 */
	public XMLBean(InputStream is) {
		docInputStream = is;
		docPath = null;
		initDocument();
	}

	/**
	 * Initializes the SAX builder
	 */
	private void initDocument() {
		try {
			SAXBuilder builder = new SAXBuilder(XMLReaders.NONVALIDATING);
			
			builder.setFeature("http://xml.org/sax/features/validation", false);
			builder.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
			builder.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
			builder.setFeature("http://xml.org/sax/features/external-general-entities", false);
			builder.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
			
			builder.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");
			builder.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");

			if (docPath != null) {
				try {
					doc = builder.build(docPath);
				} catch (Throwable t) {
					// In some environments, during maven test phase a well formed URL must be used
					// insead of ordinary path
					log.error(t.getMessage());

					try {
						doc = builder.build("file://" + docPath);
					} catch (Throwable t2) {
						log.error(t2.getMessage());
					}
				}
			} else
				try {
					doc = builder.build(docInputStream);
				} catch (Throwable t) {
					log.error(t.getMessage());
				}

			root = doc.getRootElement();
		} catch (Exception jdome) {
			log.error(jdome.getMessage());
		}
	}

	/**
	 * Finds an element using an XPATH expression
	 * 
	 * @param xpathExpression the expression to evaluate
	 * 
	 * @return the found element
	 */
	public Element findElement(String xpathExpression) {
		return findElement(xpathExpression, null);
	}

	/**
	 * Finds an element using an XPATH expression
	 * 
	 * @param xpathExpression the expression to evaluate
	 * @param namespaces map of namespaces as declared into the XML document
	 * 
	 * @return the found element
	 */
	public Element findElement(String xpathExpression, Map<String, String> namespaces) {
		try {
			XPathFactory xpath = XPathFactory.instance();
			XPathExpression<Element> expr = null;
			if (namespaces == null || namespaces.isEmpty())
				expr = xpath.compile(xpathExpression, Filters.element());
			else
				expr = xpath.compile(xpathExpression, Filters.element(), null, namespaces.keySet().stream()
						.map(key -> Namespace.getNamespace(key, namespaces.get(key))).collect(Collectors.toList()));
			return expr.evaluateFirst(doc);
		} catch (Throwable e) {
			log.error(e.getMessage());
		}
		return null;
	}

	/**
	 * Returns the root element.
	 * 
	 * @return the root element
	 */
	public Element getRootElement() {
		return root;
	}

	/**
	 * Returns a child element.
	 * 
	 * @param elemname Name of the child.
	 * 
	 * @return the child element
	 */
	public Element getChild(String elemname) {
		if (doc == null) {
			return null;
		} else {
			return root.getChild(elemname);
		}
	}

	private List<Element> getChildren(Element elem, String name) {
		List<Element> list = (List<Element>) elem.getChildren(name, elem.getNamespace());
		if (list == null || list.isEmpty())
			list = elem.getChildren(name);
		return list;
	}

	/**
	 * This method finds a child by name and attribute.
	 * 
	 * @param elemname Name of the child.
	 * @param attribute Name of the child attribute.
	 * @param value Value of the attribute.
	 * @return Child
	 */
	public Element getChild(String elemname, String attribute, String value) {
		if (doc == null) {
			return null;
		} else {
			Element temp = null;
			List<Element> list = getChildren(root, elemname);

			Iterator<Element> iter = list.iterator();
			String val = "";

			while (iter.hasNext()) {
				Element elem = iter.next();
				val = elem.getAttributeValue(attribute);

				if ((val != null) && val.equals(value)) {
					temp = elem;

					break;
				}
			}

			return temp;
		}
	}

	/**
	 * This returns the text between a begining and an ending tag.
	 * 
	 * @param elem Name of the element.
	 * @return Text of the element.
	 */
	public String getText(Element elem) {
		if (doc == null) {
			return null;
		} else {
			return elem.getText();
		}
	}

	public String getText(String elemname, String attrname, String attrvalue) {
		Element elem = getChild(elemname, attrname, attrvalue);
		return elem.getText();
	}

	/**
	 * This method returns the text of a child element characterized by
	 * elementname and attribute.
	 * 
	 * @param elemname Name of the element.
	 * @param childname Name of the child.
	 * @param attribute Name of the attribute which must element have.
	 * @param value Value of the attribute.
	 * @return Text of the child.
	 */
	public String getChildText(String elemname, String childname, String attribute, String value) {
		Element elem = getChild(elemname, attribute, value);
		elem = elem.getChild(childname);
		return getText(elem);
	}

	/**
	 * This method returns the text of all children in the format
	 * childname&lt;separator1&gt;childtext&lt;separator2&gt;
	 * 
	 * @param elemname Name of the root element
	 * @param attribute Name of the attribute which must root element have
	 * @param value Value of the attribute
	 * @param separator1 string used as first separator
	 * @param separator2 string used as second separator
	 * 
	 * @return the children's text
	 */
	@SuppressWarnings("rawtypes")
	public String getAllChildText(String elemname, String attribute, String value, String separator1,
			String separator2) {
		String result = "";
		try {
			Element elem = getChild(elemname, attribute, value);
			List list = elem.getChildren();
			Iterator iter = list.iterator();

			while (iter.hasNext()) {
				Element child = (Element) iter.next();
				result += child.getName();
				result += separator1;
				result += child.getText();
				result += separator2;
			}
		} catch (Exception ex) {
			log.error(ex.getMessage(), ex);
		}

		return result;
	}

	/**
	 * Returns an element attribute.
	 * 
	 * @param elem Name of the element.
	 * @param attrib Name of the attribute.
	 * @return Attribute
	 */
	public Attribute getAttribute(Element elem, String attrib) {
		if (doc == null) {
			return null;
		} else {
			return elem.getAttribute(attrib);
		}
	}

	/**
	 * Returns the value of an elementattribute.
	 * 
	 * @param elem Name of the element.
	 * @param attrib Name of the attribute.
	 * @return Value of the attribute.
	 */
	public String getAttributeValue(Element elem, String attrib) {

		if (doc == null) {
			return null;
		} else {
			return elem.getAttributeValue(attrib);
		}
	}

	/**
	 * This method set the value of an elementattribute.
	 * 
	 * @param elem Name of the element.
	 * @param attrib Attribute of the element.
	 * @param value New value of the attribute.
	 */
	public void setAttributeValue(Element elem, String attrib, String value) {
		elem.getAttribute(attrib).setValue(value);
	}

	public void setText(Element elem, String text) {
		elem.setText(text);
	}

	/**
	 * This method saves the xml-file connected by XMLBean. NOTE: only call this
	 * on an XMLBean _NOT_ created from an InputStream!
	 * 
	 * @return true if the file has been completely written
	 */
	public boolean writeXMLDoc() {
		// it might be that we do not have an ordinary file,
		// so we can't write to it
		if (docPath == null)
			return false;

		// Backup the file first
		File src = new File(docPath);
		File backup = new File(src.getParentFile(), src.getName() + ".back");
		try {
			FileUtils.copyFile(src, backup);
		} catch (Exception ex) {
			log.error(ex.getMessage());
		}
		log.debug("Backup saved in " + backup.getPath());

		boolean result = true;
		try {
			XMLOutputter outputter = new XMLOutputter(Format.getPrettyFormat().setIndent("  ").setLineSeparator("\n"));
			File file = new File(docPath);
			OutputStream out = new FileOutputStream(file);
			outputter.output(doc, out);
			out.close();
			log.info("Saved file " + docPath);
		} catch (Exception ex) {
			result = false;

			if (log.isWarnEnabled()) {
				log.warn(ex.getMessage());
			}
		}

		return result;
	}

	/**
	 * This method returns all direct children of the root element.
	 * 
	 * @return Childrenlist
	 */
	@SuppressWarnings("rawtypes")
	public List getRootChild() {
		if (doc == null) {
			return null;
		} else {
			return root.getChildren();
		}
	}

	/**
	 * Returns a list of all attributes from an element.
	 * 
	 * @param elem Name of the element.
	 * @return Attributelist
	 */
	@SuppressWarnings("rawtypes")
	public List getAllAttribute(Element elem) {

		if (doc == null) {
			return null;
		} else {
			return elem.getAttributes();
		}
	}

	/**
	 * Returns a list of all elements with the given element.
	 * 
	 * @param elemname name of the element
	 * 
	 * @return list of the children elements
	 */
	@SuppressWarnings("rawtypes")
	public List getAllChildren(String elemname) {
		if (doc == null) {
			return null;
		} else {
			return root.getChildren(elemname);
		}
	}

	/**
	 * Returns a list of all children with the specific name and the specific
	 * attribute value
	 * 
	 * @param elemname name of the element
	 * @param attribute name of the attribute
	 * @param value value of the attribute
	 * 
	 * @return the list of children
	 */
	@SuppressWarnings("rawtypes")
	public List getAllChildren(String elemname, String attribute, String value) {
		List<Element> list = new LinkedList<Element>();
		List elems = root.getChildren(elemname);
		Iterator iter = elems.iterator();

		while (iter.hasNext()) {
			Element elem = (Element) iter.next();

			try {
				String val = elem.getAttributeValue(attribute);

				if ((val != null) && val.equals(value)) {
					list.add(elem);
				}
			} catch (Exception e) {
				;
			}
		}

		return list;
	}

	/**
	 * Removes an element.
	 * 
	 * @param elemname Tag name of the element.
	 * 
	 * @return true if the deletion has been successful
	 */
	public boolean removeChild(String elemname) {
		return root.removeChild(elemname);
	}
}