package com.logicaldoc.core.parser;

import org.apache.xerces.parsers.AbstractSAXParser;
import org.apache.xerces.xni.Augmentations;
import org.apache.xerces.xni.NamespaceContext;
import org.apache.xerces.xni.XMLLocator;
import org.apache.xerces.xni.XMLString;
import org.apache.xerces.xni.XNIException;
import org.apache.xerces.xni.parser.XMLParserConfiguration;
import org.cyberneko.html.HTMLConfiguration;

/**
 * Helper class for HTML parsing
 */
public class HTMLSAXParser extends AbstractSAXParser {

    private StringBuffer buffer;

    public HTMLSAXParser() {
         super(new HTMLConfiguration());
    }
    
    public HTMLSAXParser(XMLParserConfiguration parserConfig) { 
        super(parserConfig);
    }

    public void startDocument(XMLLocator arg0,
                              String arg1,
                              NamespaceContext arg2,
                              Augmentations arg3) throws XNIException {
        super.startDocument(arg0, arg1, arg2, arg3);
        buffer = new StringBuffer();
    }

    public void characters(XMLString xmlString, Augmentations augmentations)
            throws XNIException {
        super.characters(xmlString, augmentations);
        buffer.append(" ");
        buffer.append(xmlString.toString());
    }

    /**
     * Returns parsed content
     *
     * @return String Parsed content
     */
    public String getContents() {
		String tmp = buffer.toString();
		if (tmp != null && tmp.length() > 1) {
			tmp = tmp.replaceAll("\\p{Blank}+", " ");
			tmp = tmp.replaceAll("\\s+", " ");
		}
		return tmp;
	}
}
