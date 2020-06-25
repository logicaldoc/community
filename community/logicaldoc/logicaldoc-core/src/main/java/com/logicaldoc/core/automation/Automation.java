package com.logicaldoc.core.automation;

import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.file.Paths;
import java.util.Date;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.runtime.RuntimeConstants;
import org.apache.velocity.runtime.RuntimeSingleton;
import org.apache.velocity.runtime.log.Log4JLogChute;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;

import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Represents the automation scripting engine
 * 
 * @author Marco Meschieri - LogicalDOC
 */
public class Automation {

	public static final String CURRENT_DATE = "CURRENT_DATE";

	public static final String PRODUCT = "product";

	public static final String LOCALE = "locale";

	public static final String SERVER_URL = "serverUrl";

	public static final String TENANT_ID = "tenantId";

	public static final String DICTIONARY = "dictionary";

	public static final String KEYS = "keys";

	public static final String CONTEXT = "context";

	public static final String PARAMETERS = "parameters";

	public static final String PARAMETERS_NAMES = "parametersnames";

	private static Logger log = LoggerFactory.getLogger(Automation.class);

	private String logTag = "AutomationEngine";

	private Locale locale = Locale.ENGLISH;

	private long tenantId = Tenant.DEFAULT_ID;

	public static synchronized void initialize() {
		try {
			if (!RuntimeSingleton.isInitialized()) {
				RuntimeSingleton.setProperty(RuntimeConstants.SET_NULL_ALLOWED, true);

				try {
					RuntimeSingleton.setProperty(RuntimeConstants.RUNTIME_LOG_LOGSYSTEM_CLASS,
							Log4JLogChute.class.getName());
				} catch (Throwable t) {
					log.warn(t.getMessage());
				}

				try {
					RuntimeSingleton.setProperty(Log4JLogChute.RUNTIME_LOG_LOG4J_LOGGER, Automation.class.getName());
				} catch (Throwable t) {
					log.warn(t.getMessage());
				}

				RuntimeSingleton.init();

				log.info("Automation has been initialized");
			}
		} catch (Throwable t) {
			log.error("Unable to initialize the automation engine", t);
		}
	}

	public Automation() {
		super();
	}

	public Automation(String logTag) {
		super();
		this.logTag = logTag;
	}

	public Automation(String logTag, Locale locale, long tenantId) {
		super();
		this.logTag = logTag;
		this.locale = locale;
		this.tenantId = tenantId;
	}

	/**
	 * Prepares the dictionary for the automation's execution. All the classes
	 * marked with @AutomationDictionary will be added and the keys of
	 * customDictionary will be merged. Moreover, these additional keys will be
	 * included.
	 * 
	 * <ol>
	 * <li>product: name of the product</li>
	 * <li>locale: the default locale</li>
	 * <li>nl: the new line</li>
	 * <li>CURRENT_DATE: the actual date</li>
	 * <li>tenantId</li>
	 * <li>dictionary: the map of all the variables</li>
	 * <li>serverUrl</li>
	 * <li>tenantId</li>
	 * </ol>
	 * 
	 * @param clientDictionary Custom keys provided by the client
	 * @return The complete dictionary to use
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private Map<String, Object> prepareDictionary(Map<String, Object> clientDictionary) {
		if (clientDictionary == null)
			clientDictionary = new ConcurrentHashMap<String, Object>();
		ConcurrentHashMap<String, Object> dictionary = new ConcurrentHashMap<String, Object>();

		/*
		 * Scan the classpath to add all the @AutomationDictionary classes
		 */
		ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(true);
		scanner.addIncludeFilter(new AnnotationTypeFilter(AutomationDictionary.class));
		for (BeanDefinition bd : scanner.findCandidateComponents("com.logicaldoc")) {
			String beanClassName = bd.getBeanClassName();
			Class beanClass = null;
			try {
				beanClass = Class.forName(beanClassName);
			} catch (ClassNotFoundException e) {
				log.error(e.getMessage(), e);
			}

			if (beanClass == null)
				continue;

			String key = beanClass.getSimpleName();
			AutomationDictionary annotation = (AutomationDictionary) beanClass
					.getAnnotation(AutomationDictionary.class);
			if (StringUtils.isNotEmpty(annotation.key()))
				key = annotation.key();

			try {
				Object instance = beanClass.getDeclaredConstructor().newInstance();
				dictionary.put(key, instance);
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		}

		/*
		 * Add some standard entries
		 */

		// The product name
		dictionary.put(PRODUCT, SystemInfo.get(tenantId).getProduct());

		// This is needed to handle new lines
		dictionary.put("nl", "\n");

		// The tenant ID
		if (!dictionary.containsKey(TENANT_ID))
			dictionary.put(TENANT_ID, this.tenantId);

		// This is the locale
		if (!clientDictionary.containsKey(LOCALE))
			clientDictionary.put(LOCALE, this.locale);

		// This is needed to format dates
		DateTool dateTool = new DateTool(I18N.getMessages((Locale) clientDictionary.get(LOCALE)).get("format_date"),
				I18N.getMessages((Locale) clientDictionary.get(LOCALE)).get("format_dateshort"));
		dictionary.put(DateTool.class.getSimpleName(), dateTool);

		// Put the current date
		dictionary.put(CURRENT_DATE, new Date());

		// Localized messages map
		dictionary.put(I18N.class.getSimpleName(),
				new I18NTool(I18N.getMessages((Locale) clientDictionary.get(LOCALE))));

		if (Context.get() != null)
			clientDictionary.put(SERVER_URL, Context.get().getProperties().get("server.url"));
		else
			try {
				clientDictionary.put(SERVER_URL, new ContextProperties().getProperty("server.url"));
			} catch (IOException e1) {

			}

		/*
		 * Merge the client dictionary
		 */
		if (clientDictionary != null && !clientDictionary.isEmpty()) {
			for (String key : clientDictionary.keySet())
				if (key != null && clientDictionary.get(key) != null)
					dictionary.put(key, clientDictionary.get(key));
		}

		// Put some static classes
		dictionary.put(Paths.class.getSimpleName(), Paths.class);
		dictionary.put(Math.class.getSimpleName(), Math.class);

		// Put a copy of all the keys(if we would traverse the keyset of the
		// dictionary we would get concurrent modification exception
		Set<String> keySet = dictionary.keySet().stream().collect(Collectors.toSet());
		dictionary.put(KEYS, keySet);

		// Add a reference to the dictionary itself
		dictionary.put(DICTIONARY, dictionary);

		// Put the application context
		if (Context.get() != null)
			dictionary.put(CONTEXT, Context.get());

		return dictionary;
	}

	private VelocityContext prepareContext(Map<String, Object> extendedDictionary) {
		initialize();

		VelocityContext context = new VelocityContext(extendedDictionary);

		return context;
	}

	/**
	 * Evaluate a given expression. The given dictionary will be integrated by
	 * {@link Automation#prepareDictionary(Map)}:
	 * 
	 * @param expression The string expression to process
	 * @param clientDictionary The dictionary to use
	 * @return The processed result
	 */
	public String evaluate(String expression, Map<String, Object> clientDictionary) {
		StringWriter writer = new StringWriter();
		try {
			if (StringUtils.isNotEmpty(expression)) {
				VelocityContext context = prepareContext(prepareDictionary(clientDictionary));

				Velocity.evaluate(context, writer, StringUtils.isNotEmpty(logTag) ? logTag : "ScriptEngine",
						expression.replace("\n", "${nl}"));
			}
			return writer.toString();
		} catch (Throwable e) {
			log.error("Error in this script: {}", expression, e);
			return expression;
		}
	}

	/**
	 * Evaluate a given expression. The given dictionary will be integrated by
	 * {@link Automation#prepareDictionary(Map)}
	 * 
	 * @param clientDictionary dictionary to be passed to the engine
	 * @param reader the reader on the automation code
	 * @param writer the writer that will receive the output
	 */
	public void evaluate(Map<String, Object> clientDictionary, Reader reader, Writer writer) {
		try {
			if (reader != null) {
				VelocityContext context = prepareContext(prepareDictionary(clientDictionary));
				Velocity.evaluate(context, writer, StringUtils.isNotEmpty(logTag) ? logTag : "ScriptEngine", reader);
			}
		} catch (Throwable e) {
			log.error("Error in the script", e);
		}
	}
}