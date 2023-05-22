package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * Abstract format converter.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public abstract class AbstractFormatConverter implements FormatConverter {

	private static Logger log = LoggerFactory.getLogger(AbstractFormatConverter.class);

	protected Map<String, String> parameters = new HashMap<>();

	protected AbstractFormatConverter() {
		loadParameters();
	}

	/**
	 * Template method that invokes
	 * {@link #convert(String, Document, File, File)}
	 */
	public final void convert(File src, File dest) throws IOException {
		this.convert(null, null, src, dest);
	}

	/**
	 * If the converter is enabled it invokes the
	 * {@link #internalConvert(String, Document, File, File)}
	 */
	@Override
	public final void convert(String sid, Document document, File src, File dest) throws IOException {
		if (!isEnabled())
			throw new IOException("Converter " + this.getClass().getSimpleName() + " is disabled");
		else
			internalConvert(sid, document, src, dest);
	}

	/**
	 * Extend this method to implement the conversion
	 */
	abstract protected void internalConvert(String sid, Document document, File src, File dest) throws IOException;

	protected ContextProperties config() {
		ContextProperties config = null;
		try {
			config = (ContextProperties) Context.get().getBean(ContextProperties.class);
		} catch (Throwable t) {
			try {
				config = new ContextProperties();
			} catch (Throwable e) {
				// Noting to do
			}
		}
		return config;
	}

	@Override
	public void loadParameters() {
		try {
			List<String> params = getParameterNames();
			for (String param : params) {
				String key = getParameterPropertyName(param);
				parameters.put(param, config().getProperty(key));
			}
		} catch (Throwable t) {
			// Noting to do
		}
	}

	@Override
	public boolean isEnabled() {
		return config().getBoolean(getParameterPropertyName("enabled"), false);
	}

	@Override
	public void setEnabled(boolean enabled) {
		config().setProperty(getParameterPropertyName("enabled"), "" + enabled);
		try {
			config().write();
		} catch (IOException e) {
			log.warn(e.getMessage());
		}
	}

	private String getParameterPropertyName(String paramName) {
		return "converter." + this.getClass().getSimpleName() + "." + paramName;
	}

	@Override
	public Map<String, String> getParameters() {
		return parameters;
	}

	@Override
	public String getParameter(String name) {
		return getParameters().get(name);
	}

	@Override
	public List<String> getParameterNames() {
		return new ArrayList<>();
	}

	@Override
	public int hashCode() {
		return this.getClass().getSimpleName().hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null)
			return false;
		return hashCode() == obj.hashCode();
	}

	/**
	 * Gets the extension for the given filename, if an alias is found then the
	 * value of the alias is returned as well. For instance if in the settings
	 * you have <code>converter.alias.eft=txt</code> then a file named test.eft
	 * will be considered a txt.
	 * 
	 * @param fileNameOrExtension file name or just the extension
	 * 
	 * @return the real extension to use
	 */
	public static String getExtension(String fileNameOrExtension) {
		String ext = fileNameOrExtension;
		if (fileNameOrExtension.contains("."))
			ext = FileUtil.getExtension(fileNameOrExtension.toLowerCase());

		String alias = Context.get().getProperties().getProperty("converter.alias." + ext);
		if (StringUtils.isNotEmpty(alias))
			ext = alias;

		return ext.toLowerCase();
	}
}