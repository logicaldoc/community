package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.XMLConstants;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.input.SAXBuilder;
import org.jdom2.input.sax.XMLReaders;
import org.jsoup.Jsoup;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.util.io.FileUtil;

/**
 * Converter to convert XML files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0
 */
public class XMLConverter extends AbstractFormatConverter {

    private static final String STYLE_SHEETS = "styleSheets";

    private static final Logger log = LoggerFactory.getLogger(XMLConverter.class);

    @Override
    public void internalConvert(String sid, AbstractDocument document, File src, File dest) throws IOException {
        String destExt = FileUtil.getExtension(dest.getName()).toLowerCase();
        File xslt = null;
        File xml = null;
        try {
            try {
                xslt = FileUtil.createTempFile("xslt", ".xsl");
                xml = FileUtil.createTempFile("xml", ".xml");
                FileUtil.copyFile(src, xml);

                // Parse the XML searching for a stylesheet
                SAXBuilder builder = new SAXBuilder(XMLReaders.NONVALIDATING);

                builder.setFeature("http://xml.org/sax/features/validation", false);
                builder.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
                builder.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
                builder.setFeature("http://xml.org/sax/features/external-general-entities", false);
                builder.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
                builder.setFeature("http://xml.org/sax/features/external-parameter-entities", false);

                org.jdom2.Document doc = builder.build(xml);

                String xsltOutFormat = getTrustedXsltOutputFormat(document, xslt, xml, builder, doc);

                if (xsltOutFormat == null) {
                    convertFromTxt(sid, document, dest, destExt, xml);
                } else {
                    convert(sid, document, dest, destExt, xslt, xml, xsltOutFormat);
                }
            } catch (IOException ioe) {
                throw ioe;
            } catch (Exception e) {
                throw new IOException(e);
            }
        } finally {
            if (xslt != null)
                FileUtil.delete(xslt);
            if (xml != null)
                FileUtil.delete(xml);
        }
    }

    /**
     * Only uses server configured, trusted stylesheets. Ignores user supplied
     * hrefs that point to arbitrary URLs.
     * 
     * @param document The document
     * @param xslt The XSLT file
     * @param xml The XML file
     * @param builder the SAX builder
     * @param domDocument the Dom representation of the XML
     * 
     * @return The XSL output format
     * 
     * @throws IOException generic I/O error
     * @throws JDOMException XML error
     */
    private String getTrustedXsltOutputFormat(
            AbstractDocument document,
            File xslt,
            File xml,
            SAXBuilder builder,
            org.jdom2.Document domDocument) throws IOException, JDOMException {

        String rootElementName = domDocument.getRootElement().getName();
        String style = getStyleByRootElement(rootElementName);
        String xsltOutFormat = null;

        if (StringUtils.isNotEmpty(style)) {
            log.debug("Using trusted stylesheet {} for converting xml file {}", style, document.getFileName());
            try {
                // Only allow classpath or local file paths, no http/ftp/jar
                if (style.startsWith("classpath:")) {
                    String path = style.substring("classpath:".length());
                    try (InputStream is = getClass().getResourceAsStream(path)) {
                        if (is == null)
                            throw new FileNotFoundException("Classpath stylesheet not found: %s".formatted(path));
                        FileUtil.writeFile(is, xslt.getAbsolutePath());
                    }
                } else if (style.startsWith("file:")) {
                    File srcXslt = new File(style.substring("file:".length()));
                    FileUtils.copyFile(srcXslt, xslt);
                } else {
                    log.warn("Rejected stylesheet {}: only classpath:/ or file:/ are allowed", style);
                    return null;
                }

                // Parse trusted XSLT to detect output method
                org.jdom2.Document xsltDoc = builder.build(xslt);
                Element root = xsltDoc.getRootElement();
                Element outputElem = root.getChild("output", root.getNamespace());
                if (outputElem != null) {
                    xsltOutFormat = outputElem.getAttributeValue("method");
                    if (xsltOutFormat != null) {
                        xsltOutFormat = xsltOutFormat.toLowerCase();
                    }
                }
            } catch (Exception e) {
                log.warn("Cannot elaborate trusted style sheet {}", style, e);
            }
        }

        return xsltOutFormat;
    }

    private void convert(
            String sid,
            AbstractDocument document,
            File dest,
            String destExt,
            File xslt,
            File xml,
            String xsltOutFormat) throws TransformerFactoryConfigurationError, IOException {
        try {
            // Create a 'safe' transformer factory
            TransformerFactory factory = TransformerFactory.newInstance();
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
            factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);

            // Block all external URI resolution from XSLT (document(), imports,
            // etc.)
            factory.setURIResolver((href, base) -> {
                throw new TransformerException("External URI resolution forbidden: %s".formatted(href));
            });

            // Use the factory to create a template containing the
            // xsl file
            Templates template = factory.newTemplates(new StreamSource(new FileInputStream(xslt)));

            // Use the template to create a transformer
            Transformer xformer = template.newTransformer();

            // Prepare the input file
            Source source = new StreamSource(new FileInputStream(xml));

            if (xsltOutFormat.equals(destExt)) {
                // The XSLT is for transforming to the wanted
                // output, so
                // just execute it
                Result result = new StreamResult(new FileOutputStream(dest));

                // Apply the xsl file to the source file and write
                // the
                // result to the output file
                xformer.transform(source, result);
            } else {
                // The XSLT is for transforming to a different
                // format so
                // execute the transformation and then the
                // conversion
                File transformedFile = FileUtil.createTempFile("trs", ".%s".formatted(xsltOutFormat));
                try (FileOutputStream fos = new FileOutputStream(transformedFile)) {
                    Result result = new StreamResult(fos);

                    // Apply the xsl file to the source file and
                    // write
                    // the result to the output file
                    xformer.transform(source, result);

                    if ("html".equalsIgnoreCase(xsltOutFormat)) {
                        // Sometimes the converter adds a not closed
                        // element <META http-equiv="Content-Type"
                        // content="text/html; charset=UTF-8">, so
                        // we
                        // must strip not closed <meta> tags
                        removeMetaTags(transformedFile);
                    }

                    FormatConverter converter = FormatConversionManager.get().getConverter(xsltOutFormat, destExt);
                    if (converter == null)
                        throw new IOException(
                                String.format("Unable to find a converter from %s to %s", xsltOutFormat, destExt));
                    converter.convert(sid, document, transformedFile, dest);
                } finally {
                    if (transformedFile != null)
                        FileUtil.delete(transformedFile);
                }
            }
        } catch (FileNotFoundException | TransformerException e) {
            log.warn("File not found or XSL tranformation error", e);
        }
    }

    private void removeMetaTags(File transformedFile) throws IOException {
        String htmlContent = FileUtil.readFile(transformedFile);
        org.jsoup.nodes.Document htmlDoc = Jsoup.parse(htmlContent);
        Elements elements = htmlDoc.select("meta");
        for (org.jsoup.nodes.Element element : elements) {
            if (!element.toString().endsWith("/>"))
                element.remove();
        }
        htmlContent = htmlDoc.html();
        FileUtil.writeFile(htmlContent, transformedFile.getAbsolutePath());
    }

    private void convertFromTxt(String sid, AbstractDocument document, File dest, String destExt, File xml)
            throws IOException {
        FormatConverter converter = FormatConversionManager.get().getConverter("txt", destExt);
        if (converter == null)
            throw new IOException(String.format("Unable to find a converter from txt to %s", destExt));
        converter.convert(sid, document, xml, dest);
    }

    private String getStyleByRootElement(String rootElementName) {
        Map<String, String> styles = loadStyleSheets();
        return styles.get(rootElementName);
    }

    private Map<String, String> loadStyleSheets() {
        Map<String, String> map = new HashMap<>();
        String value = getParameter(STYLE_SHEETS);
        if (StringUtils.isNotEmpty(value)) {
            String[] styles = value.split("\\,");
            for (String style : styles) {
                if (StringUtils.isNoneEmpty(style) && style.contains("|")) {
                    String[] tokens = style.trim().split("\\|");
                    map.put(tokens[0].trim(), tokens[1].trim());
                }
            }
        }
        return map;
    }

    @Override
    public List<String> getParameterNames() {
        return Arrays.asList(STYLE_SHEETS);
    }
}