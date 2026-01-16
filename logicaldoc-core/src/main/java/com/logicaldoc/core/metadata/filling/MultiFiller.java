package com.logicaldoc.core.metadata.filling;

import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.AbstractDocumentHistory;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.parser.Parser;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.runtime.Aspect;
import com.logicaldoc.core.runtime.RunLevel;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.spring.Context;

import jakarta.annotation.PostConstruct;

/**
 * A filler that fills using the list of fillers specified by the extension
 * point Filler
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
@Component("filler")
public class MultiFiller implements Filler {

	private static final Logger log = LoggerFactory.getLogger(MultiFiller.class);

	private List<Filler> fillers;

	/**
	 * Gets the object available in the application context
	 * 
	 * @return the instance of this object in the application context
	 */
	public static Filler get() {
		return Context.get(Filler.class);
	}

	@Override
	public void fill(ExtensibleObject object, String content, History transaction) {
		if (!RunLevel.current().aspectEnabled(Aspect.AUTOFILL))
			return;

		if (!transaction.isFill()) {
			if (log.isDebugEnabled())
				log.debug("Skiping fill of object {}", object);
			return;
		}

		if (StringUtils.isEmpty(content) && transaction.getFile() != null
				&& (transaction instanceof AbstractDocumentHistory dh)) {
			Parser parser = ParserFactory.getParser(dh.getFilename());
			if (parser != null)
				try {
					content = parser.parse(transaction.getFile(), dh.getFilename(), "UTF-8",
							(object instanceof AbstractDocument doc) ? doc.getLocale() : Locale.ENGLISH,
							transaction.getTenant());
				} catch (ParsingException e) {
					log.error("Cannot extract any text for object %s using file %s(%s)".formatted(object,
							transaction.getFile().getAbsolutePath(), dh.getFilename()), e);
				}
		}

		for (Filler filler : fillers) {
			filler.fill(object, content, transaction);
			if (log.isDebugEnabled()) {
				log.debug("Filler {} filled object {} -> {}", filler.getClass().getSimpleName(), object,
						ToStringBuilder.reflectionToString(object));
			}
		}
	}

	@PostConstruct
	public void init() {
		// Acquire the 'Filler' extensions of the core plugin sorted by position
		// and add defined fillers
		for (Extension ext : PluginRegistry.getInstance().getExtensions("logicaldoc-core", "Filler").stream()
				.sorted((e1, e2) -> Integer.valueOf(e1.getParameter("position").valueAsString())
						.compareTo(Integer.valueOf(e2.getParameter("position").valueAsString())))
				.toList()) {
			String className = ext.getParameter("class").valueAsString();

			try {
				// Try to instantiate the parser
				Object filler = Class.forName(className).getDeclaredConstructor().newInstance();
				if (filler instanceof Filler f) {
					fillers.add(f);
					log.info("Added filler {} position {}", className, ext.getParameter("position").valueAsString());
				} else {
					throw new ClassNotFoundException(
							"The specified filler %s doesn't implement DocumentListener interface"
									.formatted(className));
				}
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException e) {
				log.error(e.getMessage());
			}

		}
	}
}
