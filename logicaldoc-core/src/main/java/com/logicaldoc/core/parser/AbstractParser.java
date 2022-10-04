package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
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
		InputStream is = null;
		try {
			is = new FileInputStream(file);
			return parse(is, filename, encoding, locale, tenant, document, fileVersion);
		} catch (FileNotFoundException e) {
			log.error(e.getMessage());
			return "";
		} finally {
			if (is != null)
				try {
					is.close();
				} catch (IOException e) {
				}
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
		log.debug("Parse started");
		StringBuffer content = new StringBuffer();

		Locale lcl = locale != null ? locale : Locale.ENGLISH;
		String tnt = locale != null ? tenant : Tenant.DEFAULT_NAME;

		long timeout = 0;

		try {
			timeout = Context.get().getProperties().getInt(tenant + ".parser.timeout", 120);
		} catch (Throwable e) {
			log.warn(e.getMessage());
		}

		if (timeout <= 0)
			try {
				internalParse(input, filename, encoding, lcl, tnt, document, fileVersion, content);
			} catch (Exception e) {
				if (e instanceof ParseException)
					throw (ParseException) e;
				else
					throw new ParseException(e);
			}
		else {
			// Invoke in a separate thread
			ExecutorService executor = Executors.newSingleThreadExecutor();
			try {
				String ret = null;
				try {
					ret = executor.invokeAll(Arrays.asList(
							new InternalParseTask(input, filename, encoding, lcl, tnt, document, fileVersion, content)),
							timeout, TimeUnit.SECONDS).get(0).get();
				} catch (InterruptedException ie) {
					log.warn("Interrupted parse");
					Thread.currentThread().interrupt();
				} catch (Throwable e) {
					log.warn(e.getMessage(), e);
					if (e instanceof ParseException)
						throw (ParseException) e;
				}
				if (!"completed".equals(ret))
					throw new ParseException(ret);

			} finally {
				if (executor != null)
					executor.shutdownNow();
			}
		}

		log.debug("Parse Finished");
		return content.toString();
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

		private StringBuffer content;

		public InternalParseTask(InputStream is, String filename, String encoding, Locale locale, String tenant,
				Document document, String fileVersion, StringBuffer content) {
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
			String tenant, Document document, String fileVersion, StringBuffer output) throws Exception;

	@Override
	public int countPages(InputStream input, String filename) {
		return 1;
	}

	@Override
	public int countPages(File file, String filename) {
		InputStream is = null;
		try {
			is = new FileInputStream(file);
			return countPages(is, filename);
		} catch (FileNotFoundException e) {
			log.error(e.getMessage());
			return 1;
		} finally {
			if (is != null)
				try {
					is.close();
				} catch (IOException e) {
				}
		}
	}
}