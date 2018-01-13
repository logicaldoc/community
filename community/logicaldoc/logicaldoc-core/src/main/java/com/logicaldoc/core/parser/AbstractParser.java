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

import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.Context;

/**
 * Abstract implementation of a Parser
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 3.5
 */
public abstract class AbstractParser implements Parser {

	protected static Logger log = LoggerFactory.getLogger(AbstractParser.class);

	protected StringBuffer content = new StringBuffer();

	protected String filename;

	protected Locale locale;

	protected String encoding;

	protected String tenant = Tenant.DEFAULT_NAME;

	@Override
	public String getAuthor() {
		return "";
	}

	@Override
	public String getContent() {
		return content.toString();
	}

	@Override
	public String getTags() {
		return "";
	}

	@Override
	public String getSourceDate() {
		return "";
	}

	@Override
	public String getTitle() {
		return "";
	}

	@Override
	public String getVersion() {
		return "";
	}

	public String getFilename() {
		return filename;
	}

	public void setFilename(String filename) {
		this.filename = filename;
	}

	public Locale getLocale() {
		return locale;
	}

	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	public String getEncoding() {
		return encoding;
	}

	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}

	@Override
	public void parse(File file) {
		String enc = "UTF-8";
		InputStream is = null;
		try {
			is = new FileInputStream(file);
			setEncoding(enc);
			parse(is);
		} catch (FileNotFoundException e) {
			log.error(e.getMessage());
		} finally {
			if (is != null)
				try {
					is.close();
				} catch (IOException e) {
				}
		}
	}

	@Override
	public void parse(final InputStream input) {
		log.debug("Parse started");
		content = new StringBuffer();

		long timeout = 0;

		try {
			timeout = Context.get().getProperties().getInt(getTenant() + ".parser.timeout", 120);
		} catch (Throwable e) {
			e.printStackTrace();
			log.warn(e.getMessage());
		}

		if (timeout <= 0)
			try {
				internalParse(input);
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		else {
			// Invoke in a separate thread
			ExecutorService executor = Executors.newSingleThreadExecutor();
			String ret = null;
			try {
				ret = executor.invokeAll(Arrays.asList(new InternalParseTask(input)), timeout, TimeUnit.SECONDS).get(0)
						.get();
			} catch (Throwable e) {
				log.warn(e.getMessage(), e);
			}
			if (!"completed".equals(ret))
				log.warn("Parse timed out");
			executor.shutdownNow();
		}
		log.debug("Parse Finished");
	}

	/**
	 * Callable that performs the internal parsing.
	 */
	class InternalParseTask implements Callable<String> {
		private InputStream is;

		InternalParseTask(InputStream is) {
			this.is = is;
		}

		public String call() throws Exception {
			try {
				internalParse(is);
				return "completed";
			} catch (InterruptedException e) {
				log.warn("Parse timed out");
			}
			return null;
		}
	}

	/**
	 * Invoked by the parse method
	 */
	abstract protected void internalParse(InputStream is) throws Exception;

	@Override
	public String getTenant() {
		return tenant;
	}

	@Override
	public void setTenant(String tenant) {
		this.tenant = tenant;
	}
}