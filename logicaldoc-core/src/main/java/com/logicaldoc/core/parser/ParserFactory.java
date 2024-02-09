package com.logicaldoc.core.parser;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * This is a factory, returning a parser instance for the given file.
 * 
 * @author Michael Scholz
 */
public class ParserFactory {

	private static final String PARSER_ALIAS = "parser.alias.";

	protected static Logger log = LoggerFactory.getLogger(ParserFactory.class);

	/**
	 * This is the map of registered parsers: key is the file extension, value
	 * is the parser instance
	 */
	private static Map<String, Parser> parsers = new HashMap<>();

	/**
	 * The map of aliases. Key is the alias, value is the registered extension.
	 * (eg. test->odt(
	 */
	private static Map<String, String> aliases = new HashMap<>();

	private ParserFactory() {
	}

	/**
	 * Registers all parsers from extension points
	 */
	public static void init() {
		parsers.clear();

		// First of all register all standard parsers
		parsers.put("doc", new DOCParser());
		parsers.put("dot", new DOCParser());

		parsers.put("htm", new HTMLParser());
		parsers.put("html", new HTMLParser());

		parsers.put("pdf", new PDFParser());
		parsers.put("rtf", new RTFParser());

		// StarOffice, OpenOffice 1.0 - 1.1 extensions
		parsers.put("sxw", new OpenOfficeParser());
		parsers.put("sxc", new OpenOfficeParser());
		parsers.put("sxi", new OpenOfficeParser()); // Presentation

		// OpenOffice 2.3/3.0 extensions
		parsers.put("odt", new OpenOfficeParser());
		parsers.put("ods", new OpenOfficeParser());
		parsers.put("odp", new OpenOfficeParser());

		// OpenDocument template extensions
		parsers.put("ott", new OpenOfficeParser());
		parsers.put("ots", new OpenOfficeParser());
		parsers.put("otp", new OpenOfficeParser());

		// KOffice 1.6.x extensions
		parsers.put("kwd", new KOfficeParser());
		parsers.put("ksp", new KOfficeParser());
		parsers.put("kpr", new KOfficeParser());

		// AbiWord http://www.abisource.com/
		parsers.put("abw", new AbiWordParser());
		parsers.put("zabw", new ZABWParser()); // Compressed AbiWord document

		parsers.put("java", new TXTParser());
		parsers.put("json", new TXTParser());
		parsers.put("c", new TXTParser());
		parsers.put("cpp", new TXTParser());
		parsers.put("log", new TXTParser());
		parsers.put("txt", new TXTParser());
		parsers.put("csv", new TXTParser());
		parsers.put("dbf", new TXTParser());

		parsers.put("xml", new XMLParser());
		parsers.put("xls", new XLSParser());
		parsers.put("xlt", new XLSParser());

		// MS Office 2003 Powerpoint
		parsers.put("ppt", new PPTParser());
		parsers.put("pps", new PPTParser());
		parsers.put("pot", new PPTParser());

		// Zip and GZip
		parsers.put("zip", new ZipParser());
		parsers.put("gz", new ZipParser());
		parsers.put("tgz", new ZipParser());

		// Rar
		parsers.put("rar", new RarParser());

		// Tar
		parsers.put("tar", new TarParser());

		// 7z
		parsers.put("7z", new SevenZipParser());

		// Epub
		parsers.put("epub", new EpubParser());

		// Acquire the 'Parse' extensions of the core plugin and add defined
		// parsers
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Parser");
		for (Extension extension : exts) {
			String ext = extension.getParameter("extension").valueAsString().toLowerCase();
			String className = extension.getParameter("class").valueAsString();

			try {
				// Try to instantiate the parser
				Object parser = Class.forName(className).getDeclaredConstructor().newInstance();
				if (!(parser instanceof Parser))
					throw new ClassNotFoundException(
							String.format("The specified parser %s doesn't implement Parser interface", className));
				parsers.put(ext, (Parser) parser);
				log.info("Added new parser {} for extension {}", className, ext);
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException | SecurityException e) {
				log.error(e.getMessage());
			}

		}

		initAliases();
	}

	/**
	 * Gets the proper parser and parse the given content
	 * 
	 * @param input the input contents as stream
	 * @param filename name of the file
	 * @param encoding encoding of the stream
	 * @param locale the locale
	 * @param tenantId identifier of the tenant
	 * @param document the document the file belongs to (optional)
	 * @param fileVersion the file version being processed (optional)
	 * 
	 * @return the text extracted from the input
	 * 
	 * @throws ParseException error in the parsing
	 */
	public static String parse(InputStream input, String filename, String encoding, Locale locale, long tenantId,
			Document document, String fileVersion) throws ParseException {
		Parser parser = getParser(filename);
		TenantDAO dao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		String tenantName = dao.getTenantName(tenantId);
		return parser.parse(input, new ParseParameters(document, filename, fileVersion, encoding, locale,
				tenantName != null ? tenantName : Tenant.DEFAULT_NAME));
	}

	/**
	 * Method containing the lookup logic
	 * 
	 * @param filename name of the file
	 * 
	 * @return the right parser for the given file name
	 */
	public static Parser getParser(String filename) {
		if (parsers.isEmpty())
			init();

		String ext = filename.contains(".") ? FileUtil.getExtension(filename.trim()) : filename.trim();
		if (!StringUtils.isEmpty(ext))
			ext = ext.toLowerCase();

		Parser parser = parsers.get(ext);
		if (parser == null) {
			log.info("No registered parser for extension {}. Search for an alias.", ext);

			String alias = aliases.get(ext);
			if (StringUtils.isNotEmpty(alias)) {
				log.info("Found alias {}", alias);
				parser = parsers.get(alias);
			}
		}

		if (parser == null) {
			log.warn("Unable to find a specific parser for extension {}", ext);
			parser = new CatchAllParser();
		}

		return parser;
	}

	public static Set<String> getExtensions() {
		if (parsers.isEmpty())
			init();
		return parsers.keySet();
	}

	public static Map<String, Parser> getParsers() {
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
			StringBuilder sb = new StringBuilder();
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