package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Locale;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.Context;

/**
 * Abstract implementation of a Parser
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5
 */
public abstract class AbstractParser implements Parser {

	protected static Logger log = LoggerFactory.getLogger(AbstractParser.class);

	@Override
	public String parse(File file, String filename, String encoding, Locale locale, String tenant)
			throws ParseException {
		return parse(file, filename, encoding, locale, tenant, null, null);
	}

	@Override
	public String parse(File file, String filename, String encoding, Locale locale, String tenant, Document document,
			String fileVersion) throws ParseException {
		try (InputStream is = new FileInputStream(file);) {
			return parse(is, filename, encoding, locale, tenant, document, fileVersion);
		} catch (Throwable e) {
			log.error(e.getMessage());
			return "";
		}
	}

	@Override
	public String parse(InputStream input, String filename, String encoding, Locale locale, String tenant)
			throws ParseException {
		return parse(input, filename, encoding, locale, tenant, null, null);
	}

	@Override
	public String parse(final InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion) throws ParseException {
		if (log.isDebugEnabled())
			log.debug("Parse started");
		StringBuilder content = new StringBuilder();

		Locale lcl = getLocale(locale);
		String tnt = getTenant(locale, tenant);

		long timeout = getTimeout(tenant);

		if (timeout <= 0) {
			parseInCurrentThread(input, filename, encoding, document, fileVersion, content, lcl, tnt);
		} else {
			parseInNewThread(input, filename, encoding, document, fileVersion, content, lcl, tnt, timeout);
		}

		if (log.isDebugEnabled())
			log.debug("Parse Finished");
		return content.toString();
	}

	private int getTimeout(String tenant) {
		try {
			Context context = Context.get();
			return context != null ? Context.get().getProperties().getInt(tenant + ".parser.timeout", 120) : 120;
		} catch (Exception e) {
			return 120;
		}
	}

	private void parseInNewThread(final InputStream input, String filename, String encoding, Document document,
			String fileVersion, StringBuilder content, Locale locale, String tenant, long timeout)
			throws ParseException {
		// Invoke in a separate thread
		ExecutorService executor = Executors.newSingleThreadExecutor();
		try {
			String ret = null;
			try {
				ret = executor.invokeAll(Arrays.asList(new InternalParseTask(input, filename, encoding, locale, tenant,
						document, fileVersion, content)), timeout, TimeUnit.SECONDS).get(0).get();
			} catch (InterruptedException ie) {
				log.warn("Interrupted parse");
				Thread.currentThread().interrupt();
			} catch (Throwable e) {
				if (e instanceof ParseException)
					throw (ParseException) e;
				else
					log.warn(e.getMessage(), e);
			}
			if (!"completed".equals(ret))
				throw new ParseException(ret);
		} finally {
			if (executor != null)
				executor.shutdownNow();
		}
	}

	private void parseInCurrentThread(final InputStream input, String filename, String encoding, Document document,
			String fileVersion, StringBuilder content, Locale locale, String tenant) throws ParseException {
		try {
			internalParse(input, filename, encoding, locale, tenant, document, fileVersion, content);
		} catch (Exception e) {
			if (e instanceof ParseException)
				throw (ParseException) e;
			else
				throw new ParseException(e);
		}
	}

	private String getTenant(Locale locale, String tenant) {
		String tnt = locale != null ? tenant : Tenant.DEFAULT_NAME;
		return tnt;
	}

	private Locale getLocale(Locale locale) {
		Locale lcl = locale != null ? locale : Locale.ENGLISH;
		return lcl;
	}

	/**
	 * Callable that performs the internal parsing.
	 */
	class InternalParseTask implements Callable<String> {
		private InputStream is;

		private String filename;

		private String encoding;

		private Locale locale;

		private String tenant;

		private Document document;

		private String fileVersion;

		private StringBuilder content;

		public InternalParseTask(InputStream is, String filename, String encoding, Locale locale, String tenant,
				Document document, String fileVersion, StringBuilder content) {
			super();
			this.is = is;
			this.filename = filename;
			this.encoding = encoding;
			this.locale = locale;
			this.tenant = tenant;
			this.content = content;
			this.document = document;
			this.fileVersion = fileVersion;
		}

		public String call() throws ParseException {
			try {
				internalParse(is, filename, encoding, locale, tenant, document, fileVersion, content);
				return "completed";
			} catch (InterruptedException e) {
				try {
					String message = "Timeout while parsing document " + document;
					log.warn(message);
					throw new ParseException(message);
				} finally {
					Thread.currentThread().interrupt();
				}
			} catch (ParseException pe) {
				throw pe;
			} catch (Exception ee) {
				throw new ParseException(ee.getMessage(), ee);
			}
		}
	}

	/**
	 * Invoked by the parse method
	 */
	abstract protected void internalParse(InputStream is, String filename, String encoding, Locale locale,
			String tenant, Document document, String fileVersion, StringBuilder output) throws Exception;

	@Override
	public int countPages(InputStream input, String filename) {
		return 1;
	}

	@Override
	public int countPages(File file, String filename) {
		try (InputStream is = new FileInputStream(file);) {
			return countPages(is, filename);
		} catch (Throwable e) {
			log.error(e.getMessage());
			return 1;
		}
	}
}