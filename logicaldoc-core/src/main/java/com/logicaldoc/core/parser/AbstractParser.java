package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
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
		try (InputStream is = new FileInputStream(file);) {
			return parse(is, new ParseParameters(document, filename, fileVersion, encoding, locale, tenant));
		} catch (IOException e) {
			log.error(e.getMessage());
			return "";
		}
	}

	@Override
	public String parse(InputStream input, String filename, String encoding, Locale locale, String tenant)
			throws ParseException {
		return parse(input, new ParseParameters(null, filename, null, encoding, locale, tenant));
	}

	@Override
	public String parse(final InputStream input, ParseParameters parameters) throws ParseException {
		if (log.isDebugEnabled())
			log.debug("Parse started");
		StringBuilder content = new StringBuilder();

		parameters.setLocale(getLocale(parameters.getLocale()));
		parameters.setTenant(getTenant(parameters.getLocale(), parameters.getTenant()));

		long timeout = getTimeout(parameters.getTenant());

		if (timeout <= 0) {
			parseInCurrentThread(input, parameters, content);
		} else {
			parseInNewThread(input, parameters, content, timeout);
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

	private void parseInNewThread(final InputStream input, ParseParameters parameters, StringBuilder content,
			long timeout) throws ParseException {
		// Invoke in a separate thread
		ExecutorService executor = Executors.newSingleThreadExecutor();
		try {
			String ret = null;
			try {
				ret = executor.invokeAll(Arrays.asList(new InternalParseTask(input, parameters, content)), timeout,
						TimeUnit.SECONDS).get(0).get();
			} catch (InterruptedException ie) {
				log.warn("Interrupted parse");
				Thread.currentThread().interrupt();
			} catch (Exception e) {
				log.warn(e.getMessage(), e);
			}
			if (!"completed".equals(ret))
				throw new ParseException(ret);
		} finally {
			if (executor != null)
				executor.shutdownNow();
		}
	}

	private void parseInCurrentThread(final InputStream input, ParseParameters parameters, StringBuilder content)
			throws ParseException {
		try {
			internalParse(input, parameters, content);
		} catch (ParseException pe) {
			throw pe;
		} catch (Exception e) {
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

		private ParseParameters parameters;

		private StringBuilder content;

		public InternalParseTask(InputStream is, ParseParameters parameters, StringBuilder content) {
			super();
			this.is = is;
			this.parameters = parameters;
			this.content = content;
		}

		public String call() throws ParseException {
			try {
				internalParse(is, parameters, content);
				return "completed";
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
	abstract protected void internalParse(InputStream is, ParseParameters parameters, StringBuilder output)
			throws IOException, ParseException;

	@Override
	public int countPages(InputStream input, String filename) {
		return 1;
	}

	@Override
	public int countPages(File file, String filename) {
		try (InputStream is = new FileInputStream(file);) {
			return countPages(is, filename);
		} catch (IOException e) {
			log.error(e.getMessage());
			return 1;
		}
	}
}