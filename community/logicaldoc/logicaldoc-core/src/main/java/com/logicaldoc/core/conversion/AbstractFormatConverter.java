package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Abstract format converter.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public abstract class AbstractFormatConverter implements FormatConverter {

	private static Logger log = LoggerFactory.getLogger(AbstractFormatConverter.class);

	protected Map<String, String> parameters = new HashMap<String, String>();

	public AbstractFormatConverter() {
		loadParameters();
	}

	/**
	 * Template method that invokes {@link #convert(null, null, File, File)}
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

	@Override
	public void loadParameters() {
		try {
			ContextProperties config = Context.get().getProperties();
			List<String> params = getParameterNames();
			for (String param : params) {
				String key = getParameterPropertyName(param);
				parameters.put(param, config.getProperty(key));
			}
		} catch (Throwable t) {
		}
	}

	@Override
	public boolean isEnabled() {
		ContextProperties config = (ContextProperties) Context.get().getBean(ContextProperties.class);
		return config.getBoolean(getParameterPropertyName("enabled"), false);
	}

	@Override
	public void setEnabled(boolean enabled) {
		ContextProperties config = (ContextProperties) Context.get().getBean(ContextProperties.class);
		config.setProperty(getParameterPropertyName("enabled"), "" + enabled);
		try {
			config.write();
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
		return new ArrayList<String>();
	}

	@Override
	public int hashCode() {
		return this.getClass().getSimpleName().hashCode();
	}
}