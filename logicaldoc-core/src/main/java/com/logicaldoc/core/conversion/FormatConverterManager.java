package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.commons.lang.StringUtils;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.core.security.user.UserHistoryDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.util.DocUtil;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * Manager class used to handle format converters. For each couple
 * inFormat-outFormat you may have different possible converters, they are taken
 * by the <b>FormatConverter</b> extension point. The actual converter uses is
 * the one pointed by the context property names converter.inExt-outExt.
 * 
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.3
 */
@Component("formatConverterManager")
public class FormatConverterManager {

	/*
	 * The suffix for the document's resource that represents the PDF conversion
	 */
	public static final String PDF_CONVERSION_SUFFIX = "conversion.pdf";

	private static final Logger log = LoggerFactory.getLogger(FormatConverterManager.class);

	@Resource(name = "Store")
	protected Store store;

	@Resource(name = "TenantDAO")
	protected TenantDAO tenantDao;

	@Resource(name = "documentManager")
	protected DocumentManager documentManager;

	@Resource(name = "ContextProperties")
	protected ContextProperties config;

	// Key is the src_extension-dst_extension, value is a collection of
	// converters
	private Map<String, List<FormatConverter>> converters = new HashMap<>();

	// All the available converters
	private Map<String, FormatConverter> availableConverters = new HashMap<>();

	/**
	 * Retrieves the content of the Pdf conversion. If the Pdf conversion is not
	 * available in the store, it is created.
	 * 
	 * @param document The document to be processed
	 * @param fileVersion The file version(optional)
	 * @param sid (optional)
	 * @return The content of the PDF as bytes
	 * 
	 * @throws IOException If something went wrong
	 */
	public byte[] getPdfContent(Document document, String fileVersion, String sid) throws IOException {
		String resource = store.getResourceName(document.getId(), getSuitableFileVersion(document, fileVersion),
				PDF_CONVERSION_SUFFIX);
		if ("pdf".equals(AbstractFormatConverter.getExtension(document.getFileName())))
			resource = store.getResourceName(document, getSuitableFileVersion(document, fileVersion), null);
		if (!store.exists(document.getId(), resource))
			convertToPdf(document, fileVersion, sid);
		return store.getBytes(document.getId(), resource);
	}

	/**
	 * Write the content of the Pdf conversion into a file. If the Pdf
	 * conversion is not available in the store, it is created.
	 * 
	 * @param document The document to be processed
	 * @param fileVersion The file version(optional)
	 * @param out the output file
	 * @param sid (optional)
	 * 
	 * @throws IOException If something went wrong
	 */
	public void writePdfToFile(Document document, String fileVersion, File out, String sid) throws IOException {
		String resource = store.getResourceName(document.getId(), getSuitableFileVersion(document, fileVersion),
				PDF_CONVERSION_SUFFIX);
		if ("pdf".equals(AbstractFormatConverter.getExtension(document.getFileName())))
			resource = store.getResourceName(document.getId(), getSuitableFileVersion(document, fileVersion), null);
		if (!store.exists(document.getId(), resource))
			convertToPdf(document, fileVersion, sid);
		store.writeToFile(document.getId(), resource, out);
	}

	/**
	 * Creates the pdf for the specified document and file version (suffix
	 * PDF_CONVERSION_SUFFIX). If the Pdf conversion already exists it, nothing
	 * happens.
	 * 
	 * @param document The document to be processed
	 * @param fileVersion The file version(optional)
	 * @param sid (optional)
	 * 
	 * @throws IOException If something went wrong
	 */
	public void convertToPdf(Document document, String fileVersion, String sid) throws IOException {
		String fileName = DocUtil.getFileName(document, fileVersion);

		if ("pdf".equals(AbstractFormatConverter.getExtension(fileName))) {
			log.debug("Document {} itself is a Pdf", document.getId());
			return;
		}

		String resource = store.getResourceName(document, getSuitableFileVersion(document, fileVersion),
				PDF_CONVERSION_SUFFIX);

		if (store.size(document.getId(), resource) > 0L) {
			log.debug("Pdf conversion already available for document {}", document.getId());
			return;
		}

		FormatConverter converter = getConverter(fileName, "pdf");
		if (converter == null)
			return;

		// Prepare I/O files
		File src = null;
		File dest = FileUtil.createTempFile("conversion", ".pdf");

		try {
			src = writeToFile(document, fileVersion);
			if (src == null || src.length() == 0)
				throw new IOException(
						String.format("Unexisting source file,  document: %s - %s", document.getId(), fileName));

			converter.convert(sid, document, src, dest);

			if (dest == null || dest.length() == 0)
				throw new IOException(
						String.format("The converter %s was unable to convert as pdf the document: %s - %s",
								converter.getClass().getSimpleName(), document.getId(), fileName));

			store.store(dest, document.getId(), resource);
		} finally {
			// Delete temporary resources
			FileUtil.delete(src);
			FileUtil.delete(dest);
		}
	}

	/**
	 * Shortcut for convertToPdf(document, null, transaction)
	 * 
	 * @param document The document to be processed
	 * @param sid identifier of the session
	 * 
	 * @throws IOException If something went wrong
	 */
	public void convertToPdf(Document document, String sid) throws IOException {
		convertToPdf(document, null, sid);
	}

	/**
	 * Converts a document into another format and saves the resulting file in
	 * the same folder
	 * 
	 * @param document The document to be processed
	 * @param fileVersion The file version(optional)
	 * @param format the extension used to define the output format
	 * @param transaction details of the current session
	 *
	 * @return the document that represents the conversion
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 */
	public Document convert(Document document, String fileVersion, String format, DocumentHistory transaction)
			throws IOException, PersistenceException {
		String fileName = DocUtil.getFileName(document, fileVersion);
		File out = null;
		FormatConverter converter = null;
		try {
			out = FileUtil.createTempFile("conv", "." + format);
			converter = getConverter(fileName, format);

			if (converter == null)
				throw new IOException(String.format("No converter available  for %s into format %s", fileName, format));

			log.debug("Using converter {} for {}", converter.getClass().getSimpleName(), fileName);

			convertToFile(document, fileVersion, out, transaction);
			if (out.exists() && out.length() > 0) {
				Document docVO = new Document();
				docVO.setFileName(FileUtil.getBaseName(fileName) + "." + format);
				docVO.setFolder(document.getFolder());
				docVO.setLanguage(document.getLanguage());

				DocumentHistory createHistory = new DocumentHistory(transaction);
				createHistory.setComment(null);
				return documentManager.create(out, docVO, createHistory);
			} else
				throw new IOException("The conversion was not done");
		} finally {
			// Delete temporary resources
			FileUtil.delete(out);
		}
	}

	/**
	 * Converts a document and writes the content of the conversion into a file.
	 * 
	 * @param document The document to be processed
	 * @param fileVersion The file version(optional)
	 * @param out the target file, the extension of this filename is used to
	 *        detect the output format
	 * @param transaction informations about the session
	 * 
	 * @throws IOException if an error happens during the conversion
	 */
	public void convertToFile(Document document, String fileVersion, File out, DocumentHistory transaction)
			throws IOException {
		String fileName = DocUtil.getFileName(document, fileVersion);
		FormatConverter converter = getConverter(fileName, out.getName());
		if (converter == null)
			return;

		FileUtil.delete(out);

		// Prepare I/O files
		File src = null;
		try {
			src = writeToFile(document, fileVersion);
			if (src == null || src.length() == 0)
				throw new IOException(
						String.format("Unexisting source file,  document: %s - %s", document.getId(), fileName));

			converter.convert(transaction != null ? transaction.getSessionId() : null, document, src, out);
			if (out.length() <= 0)
				throw new IOException(String.format("The converter %s was unable to convert document: %s",
						converter.getClass().getSimpleName(), document.getId() + " - " + fileName));

			if (transaction != null) {
				transaction.setEvent(DocumentEvent.CONVERTED.toString());
				transaction.setComment("format: " + FileUtil.getExtension(out.getName()));
				DocumentDAO dao = Context.get(DocumentDAO.class);
				try {
					dao.initialize(document);
					dao.store(document, transaction);
				} catch (PersistenceException e) {
					log.warn(e.getMessage(), e);
				}
			}
		} finally {
			// Delete temporary resources
			FileUtil.delete(src);
		}
	}

	/**
	 * Converts a file into a different format
	 * 
	 * @param in input file (the filename extension determines the source
	 *        format, in case you do not use inFilename)
	 * @param inFilename file name of the input
	 * @param out output file (if outFormat is null, the filename extension
	 *        determines the output format)
	 * @param outFormat extension of the output
	 * @param sid session ID (optional)
	 * 
	 * @throws IOException if an error happens during the conversion
	 */
	public void convertFile(File in, String inFilename, File out, String outFormat, String sid) throws IOException {
		FormatConverter converter = getConverter(inFilename, outFormat);
		if (converter == null)
			throw new IOException("Converter not found");

		if (!in.exists() || in.length() == 0)
			throw new IOException(String.format("Unexisting source file %s", in));

		FileUtil.delete(out);

		converter.convert(in, out);

		if (out == null || !out.exists() || out.length() == 0)
			throw new IOException(String.format("Converter %s has not been able to convert file %s",
					converter.getClass().getSimpleName(), in.getPath()));

		// Register this event in the user's history
		if (StringUtils.isNotEmpty(sid)) {
			Session session = SessionManager.get().get(sid);
			if (session != null) {
				UserHistory history = new UserHistory();

				history.setSession(session);
				history.setEvent(UserEvent.FILE_CONVERSION.toString());
				history.setFilename(FileUtil.getBaseName(inFilename) + "." + outFormat.toLowerCase());
				history.setFileSize(in.length());
				history.setFilenameOld(inFilename);
				history.setComment(String.format("%s -> %s", history.getFilenameOld(), history.getFilename()));
				history.setIp(session.getClient().getAddress());

				UserHistoryDAO dao = Context.get(UserHistoryDAO.class);
				try {
					dao.store(history);
				} catch (PersistenceException e) {
					log.warn(e.getMessage(), e);
				}
			}
		}
	}

	protected String getTenantName(Document document) {
		String tenantName = "default";
		try {
			Tenant tenant = tenantDao.findById(document.getTenantId());
			tenantName = tenant.getName();
		} catch (Exception t) {
			log.error(t.getMessage());
		}
		return tenantName;
	}

	/**
	 * Returns the available output formats for the given input format
	 * 
	 * @param srcFilename name of the file
	 * 
	 * @return identifiers of the possible output extensions
	 */
	public List<String> getEnabledOutputFormats(String srcFilename) {
		String inExt = AbstractFormatConverter.getExtension(srcFilename);
		List<String> formats = new ArrayList<>();
		for (String key : getConverters().keySet()) {
			String[] inOut = key.split("-");
			FormatConverter assignedConverter = getConverter(srcFilename, inOut[1]);
			// The actually assigned converter must be anabled
			if (!formats.contains(inOut[1]) && assignedConverter!=null && assignedConverter.isEnabled() && !inExt.equalsIgnoreCase(inOut[1])
					&& inExt.equalsIgnoreCase(inOut[0]))
				formats.add(inOut[1]);
		}
		return formats;
	}

	/**
	 * Returns all the possible output formats for the given input format
	 * 
	 * @param inFileName name of the file to convert
	 * 
	 * @return list of possible output formats
	 */
	public List<String> getAllOutputFormats(String inFileName) {
		String inExt = inFileName;
		if (inFileName.contains("."))
			inExt = FileUtil.getExtension(inFileName);
		List<String> formats = new ArrayList<>();
		for (String key : getConverters().keySet()) {
			String[] inOut = key.split("-");
			if (!formats.contains(inOut[1]) && (inExt.equalsIgnoreCase(inOut[0]) || "*".equals(inOut[0]))
					&& !inExt.equalsIgnoreCase(inOut[1]))
				formats.add(inOut[1]);
		}
		return formats;
	}

	/**
	 * Returns all the possible input formats
	 * 
	 * @return the collection of all available input formats
	 */
	public List<String> getAvailableInputFormats() {
		List<String> formats = new ArrayList<>();
		for (String key : getConverters().keySet()) {
			String[] inOut = key.split("-");
			if (!formats.contains(inOut[0]) && !"*".equals(inOut[0]))
				formats.add(inOut[0]);
		}
		Collections.sort(formats);
		return formats;
	}

	/**
	 * Returns the list of possible converters for a given in and out format
	 * 
	 * @param inFileName input file name
	 * @param outFileName conversion file name
	 * 
	 * @return the collection of all installed format converters
	 */
	public List<FormatConverter> getAvailableConverters(String inFileName, String outFileName) {
		String key = composeKey(inFileName, outFileName);
		return getConverters().get(key) != null ? getConverters().get(key) : new ArrayList<>();
	}

	/**
	 * Returns the list of possible converters
	 * 
	 * @return the collection of all installed format converters
	 */
	public Collection<FormatConverter> getAllConverters() {
		return availableConverters.values();
	}

	/**
	 * Loads the proper converter for the passed file names. The right converter
	 * used is defined in the configuration parameter converter.composeKey()
	 * 
	 * @param inFileName file name of the input content
	 * @param outFileName file name of the conversion
	 * 
	 * @return The right format converter
	 */
	public FormatConverter getConverter(String inFileName, String outFileName) {
		if (AbstractFormatConverter.getExtension(inFileName).equals(AbstractFormatConverter.getExtension(outFileName)))
			return new NoConversionConverter();

		String inOutkey = composeKey(inFileName, outFileName);

		List<FormatConverter> formatConverters = getConverters().get(inOutkey);
		if (formatConverters == null || formatConverters.isEmpty())
			formatConverters = getConverters().get("*-pdf");
		if (formatConverters == null || formatConverters.isEmpty())
			log.warn("No format converter for file {}", inFileName);

		// Get the first available and enabled converter
		FormatConverter converter = formatConverters.stream().filter(c -> c.isEnabled()).findFirst()
				.orElse(new NotAvailableConverter());

		// Check if a special binding is configured and points to an enabled
		// converter
		String currentConverter = config.getProperty("converter." + inOutkey);

		if (StringUtils.isNotEmpty(currentConverter) && formatConverters.size() > 1)
			converter = formatConverters.stream()
					.filter(conv -> conv.getClass().getName().equals(currentConverter) && conv.isEnabled()).findFirst()
					.orElse(converter);

		if (converter != null)
			converter.loadParameters();

		return converter;
	}

	/**
	 * Creates the key as &lt;in_extension&gt;-&lt;out_extension&gt;
	 * 
	 * @param inFileName file name of the input content
	 * @param outFileName file name of the conversion
	 * 
	 * @return the key
	 */
	private String composeKey(String inFileName, String outFileName) {
		String inExt = AbstractFormatConverter.getExtension(inFileName).toLowerCase();
		String outExt = AbstractFormatConverter.getExtension(outFileName).toLowerCase();
		return inExt + "-" + outExt;
	}

	/**
	 * Write a document into a temporary file.
	 * 
	 * @param document the document
	 * @param fileVersion version of the file
	 * 
	 * @return the temporary file containing the document's contents
	 * 
	 * @throws IOException raised if the file cannot be written
	 */
	private File writeToFile(Document document, String fileVersion) throws IOException {
		File target = FileUtil.createTempFile("scr",
				"." + AbstractFormatConverter.getExtension(document.getFileName()));
		String fver = getSuitableFileVersion(document, fileVersion);
		String resource = store.getResourceName(document, fver, null);
		store.writeToFile(document.getId(), resource, target);
		return target;
	}

	/**
	 * Returns the fileVersion in case this is not null or
	 * document.getFileVersion() otherwise
	 * 
	 * @param document the document
	 * @param fileVersion version of the file
	 * 
	 * @return the file version
	 */
	private String getSuitableFileVersion(Document document, String fileVersion) {
		String fver = fileVersion;
		if (fver == null)
			fver = document.getFileVersion();
		return fver;
	}

	/**
	 * Initializes the converters map
	 */
	public synchronized void init() {
		if (!converters.isEmpty())
			return;

		// Acquire the 'FormatConverter' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "FormatConverter");

		for (Extension ext : exts) {
			String className = ext.getParameter("class").valueAsString();

			// Try to instantiate the converter
			Object converter;
			try {
				Class<?> clazz = Class.forName(className);
				converter = clazz.getDeclaredConstructor().newInstance();
				if (!(converter instanceof FormatConverter))
					throw new ClassNotFoundException(String.format(
							"The specified converter %s doesn't implement FormatConverter interface", className));
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException e) {
				log.warn(e.getMessage(), e);
				continue;
			}

			FormatConverter cnvrt = (FormatConverter) converter;
			for (String name : cnvrt.getParameterNames())
				cnvrt.getParameters().put(name, null);

			String in = ext.getParameter("in").valueAsString().toLowerCase();
			String out = ext.getParameter("out").valueAsString().toLowerCase();
			String[] ins = in.split(",");
			String[] outs = out.split(",");
			updateConvertersMap(cnvrt, ins, outs);
			log.info("Registered format converter {} for extensions {}", className, in);

			// Save the converter in the list of available converters
			if (!availableConverters.containsKey(cnvrt.getClass().getSimpleName()))
				availableConverters.put(cnvrt.getClass().getSimpleName(), cnvrt);
		}
	}

	private void updateConvertersMap(FormatConverter cnvrt, String[] ins, String[] outs) {
		for (String i : ins)
			for (String o : outs) {
				String key = composeKey(i, o);
				List<FormatConverter> convList = converters.computeIfAbsent(key, k -> new ArrayList<>());
				if (!convList.contains(cnvrt))
					convList.add(cnvrt);
			}
	}

	public Map<String, List<FormatConverter>> getConverters() {
		if (converters.isEmpty())
			init();
		return converters;
	}
}