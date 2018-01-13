package com.logicaldoc.core.parser;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import net.sf.jmimemagic.Magic;
import net.sf.jmimemagic.MagicMatch;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.parser.wordperfect.WordPerfectParser;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.IOUtil;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * This is a factory, returning a parser instance for the given file. Created on
 * 4. November 2003, 21:54
 * 
 * @author Michael Scholz
 */
public class ParserFactory {

	private static final String PARSER_ALIAS = "parser.alias.";

	protected static Logger log = LoggerFactory.getLogger(ParserFactory.class);

	/**
	 * This is the map of registered parsers: key is the file extension, value
	 * is the parser class
	 */
	private static Map<String, Class> parsers = new HashMap<String, Class>();

	/**
	 * The map of aliases. Key is the alias, value is the registered extension.
	 * (eg. test->odt
	 */
	private static Map<String, String> aliases = new HashMap<String, String>();

	/**
	 * Registers all parsers from extension points
	 */
	public static void init() {
		parsers.clear();

		// First of all register all standard parsers
		parsers.put("doc", DOCParser.class);
		parsers.put("dot", DOCParser.class);

		parsers.put("htm", HTMLParser.class);
		parsers.put("html", HTMLParser.class);

		parsers.put("pdf", PDFParser.class);
		parsers.put("rtf", RTFParser.class);

		// StarOffice, OpenOffice 1.0 - 1.1 extensions
		parsers.put("sxw", OpenOfficeParser.class);
		parsers.put("sxc", OpenOfficeParser.class);
		parsers.put("sxi", OpenOfficeParser.class); // Presentation

		// OpenOffice 2.3/3.0 extensions
		parsers.put("odt", OpenOfficeParser.class);
		parsers.put("ods", OpenOfficeParser.class);
		parsers.put("odp", OpenOfficeParser.class);

		// OpenDocument template extensions
		parsers.put("ott", OpenOfficeParser.class);
		parsers.put("ots", OpenOfficeParser.class);
		parsers.put("otp", OpenOfficeParser.class);

		// KOffice 1.6.x extensions
		parsers.put("kwd", KOfficeParser.class);
		parsers.put("ksp", KOfficeParser.class);
		parsers.put("kpr", KOfficeParser.class);

		// WordPerfect
		parsers.put("wpd", WordPerfectParser.class);

		// AbiWord http://www.abisource.com/
		parsers.put("abw", AbiWordParser.class);
		parsers.put("zabw", ZABWParser.class); // Compressed AbiWord document

		parsers.put("txt", TXTParser.class);
		parsers.put("csv", TXTParser.class);
		parsers.put("dbf", TXTParser.class);
		parsers.put("xml", XMLParser.class);
		parsers.put("xls", XLSParser.class);
		parsers.put("xlt", XLSParser.class);

		// MS Office 2003 Powerpoint
		parsers.put("ppt", PPTParser.class);
		parsers.put("pps", PPTParser.class);
		parsers.put("pot", PPTParser.class);

		// Acquire the 'Parse' extensions of the core plugin and add defined
		// parsers
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Parser");
		for (Extension extension : exts) {
			String ext = extension.getParameter("extension").valueAsString().toLowerCase();
			String className = extension.getParameter("class").valueAsString();
			try {
				Class clazz = Class.forName(className);
				// Try to instantiate the parser
				Object parser = clazz.newInstance();
				if (!(parser instanceof Parser))
					throw new Exception("The specified parser " + className + " doesn't implement Parser interface");
				parsers.put(ext, clazz);
				log.info("Added new parser " + className + " for extension " + ext);
			} catch (Throwable e) {
				log.error(e.getMessage());
			}
		}

		initAliases();
	}

	public static Parser getParser(File file, String filename, Locale locale, String encoding, long tenantId) {
		Parser parser = detectParser(file, null, filename, locale, encoding, tenantId);
		parser.parse(file);
		return parser;
	}

	/**
	 * Internal method containing the lookup logic. can be invoked with a File
	 * OR an InputStream.
	 * 
	 * @param file
	 * @param is
	 * @param filename
	 * @param locale
	 * @param encoding
	 * @return
	 */
	protected static Parser detectParser(File file, InputStream is, String filename, Locale locale, String encoding,
			long tenantId) {
		if (parsers.isEmpty())
			init();

		String ext = filename != null ? FilenameUtils.getExtension(filename) : null;
		if (StringUtils.isEmpty(ext) && file != null) {
			String fileName = file.getName().toLowerCase();
			ext = FilenameUtils.getExtension(fileName);
		} else {
			ext = ext.toLowerCase();
		}
		if (StringUtils.isEmpty(ext))
			ext = "txt";

		Parser parser = null;
		Class parserClass = parsers.get(ext);
		if (parserClass != null) {
			try {
				parser = (Parser) parserClass.newInstance();
			} catch (Exception e) {
				log.error(e.getMessage());
				parser = new DummyParser();
			}
		} else {
			log.info("No registered parser for extension " + ext + ". Search for alias.");

			String alias = aliases.get(ext);
			if (StringUtils.isNotEmpty(alias)) {
				log.info("Found alias " + alias);
				parserClass = parsers.get(alias);
				if (parserClass != null) {
					try {
						parser = (Parser) parserClass.newInstance();
					} catch (Exception e) {
						log.error(e.getMessage());
						parser = new DummyParser();
					}
				}
			} else {
				log.warn("No registered parser for extension " + ext);
				try {
					MagicMatch match = null;
					if (file != null)
						match = Magic.getMagicMatch(file, true);
					else {
						InputStream limited = IOUtil.getLimitedStream(is, 2048);
						match = Magic.getMagicMatch(IOUtils.toByteArray(limited), true);
					}
					if ("text/plain".equals(match.getMimeType())) {
						log.warn("Try to parse the file as plain text");
						parser = new TXTParser();
					} else {
						parser = new DummyParser();
					}
				} catch (Exception e) {
					log.error(e.getMessage());
					parser = new DummyParser();
				}
			}
		}
		parser.setFilename(filename);
		parser.setLocale(locale);
		parser.setEncoding(encoding);

		if (tenantId != Tenant.DEFAULT_ID) {
			TenantDAO dao = (TenantDAO) Context.get().getBean(TenantDAO.class);
			Tenant tenant = dao.findById(tenantId);
			parser.setTenant(tenant.getName());
		}

		return parser;
	}

	public static Parser getParser(InputStream is, String filename, Locale locale, String encoding, long tenantId) {
		try {
			Parser parser = detectParser(null, is, filename, locale, encoding, tenantId);
			parser.parse(is);
			return parser;
		} finally {
			try {
				if (is != null)
					is.close();
			} catch (Throwable e) {

			}
		}
	}

	public static Parser getParser(String filename, String tenantName) {
		if (parsers.isEmpty())
			init();

		String ext = FilenameUtils.getExtension(filename).toLowerCase();

		Parser parser = null;
		Class parserClass = parsers.get(ext);
		if (parserClass != null) {
			try {
				parser = (Parser) parserClass.newInstance();
			} catch (Exception e) {
				log.error(e.getMessage());
				parser = new TXTParser();
			}
		} else {
			log.warn("No registered parser for extension " + ext);
			parser = new DummyParser();
		}
		parser.setFilename(filename);

		if (tenantName != null && !tenantName.equals(Tenant.DEFAULT_NAME))
			parser.setTenant(tenantName);

		return parser;
	}

	public static Set<String> getExtensions() {
		if (parsers.isEmpty())
			init();
		return parsers.keySet();
	}

	public static Map<String, Class> getParsers() {
		if (parsers.isEmpty())
			init();
		return parsers;
	}

	/**
	 * Adds new aliases for the specified extension.
	 * <p>
	 * Each alias is saved as property <b>parser.alias.&lt;ext&gt;</b><br>
	 * example: parser.alias.odt = test, acme<br>
	 * In this case an extension 'test' will be treated as 'odt'
	 * 
	 * @param ext Must be one of the registered extensions
	 * @param aliases Array of extension aliases (eg. test, acme ...)
	 */
	public static void setAliases(String ext, String[] aliases) {
		ContextProperties config = Context.get().getProperties();
		String pAlias = PARSER_ALIAS + ext.toLowerCase();
		if (aliases == null || aliases.length == 0) {
			config.setProperty(pAlias, "");
		} else {
			// Save as comma-separated values
			StringBuffer sb = new StringBuffer();
			for (String alias : aliases) {
				sb.append(",");
				sb.append(alias.trim());
			}
			config.setProperty(pAlias, sb.substring(1));
		}

		try {
			config.write();
		} catch (IOException e) {
			log.warn("Unable to save context properties.", e);
		}

		initAliases();
	}

	private static void initAliases() {
		aliases.clear();

		if (Context.get() == null)
			return;

		ContextProperties config = Context.get().getProperties();
		for (Object key : config.keySet()) {
			if (key.toString().startsWith(PARSER_ALIAS)) {
				String ext = key.toString().substring(PARSER_ALIAS.length());

				StringTokenizer st = new StringTokenizer(config.getProperty(key.toString()), ",", false);
				while (st.hasMoreElements()) {
					String alias = (String) st.nextElement();
					aliases.put(alias.toLowerCase().trim(), ext);
				}
			}
		}
	}
}