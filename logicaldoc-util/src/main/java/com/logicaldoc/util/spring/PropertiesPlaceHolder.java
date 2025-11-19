package com.logicaldoc.util.spring;

import java.io.IOException;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertiesPropertySource;
import org.springframework.core.env.PropertySource;
import org.springframework.core.io.ClassPathResource;

/**
 * Extends the standard property placeholder to retrieve properties from the
 * database.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
public class PropertiesPlaceHolder extends PropertySourcesPlaceholderConfigurer {

	private static final String LD_CONFIG = "ld.config";
	
	private static final Logger log = LoggerFactory.getLogger(PropertiesPlaceHolder.class);

	public PropertiesPlaceHolder() {
		super();

		String config = StringUtils.defaultString(System.getProperty(LD_CONFIG), "context.properties");
		log.info("Take configuration from resource {}", config);

		setLocation(new ClassPathResource(config));
	}

	@Override
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		MutablePropertySources propertySources = new MutablePropertySources();
		try {
			PropertySource<?> localPropertySource = new PropertiesPropertySource(LOCAL_PROPERTIES_PROPERTY_SOURCE_NAME,
					mergeProperties());
			if (this.localOverride) {
				propertySources.addFirst(localPropertySource);
				propertySources.addLast(new DatabasePropertySource());
			} else {
				propertySources.addFirst(new DatabasePropertySource());
				propertySources.addLast(localPropertySource);
			}
			setPropertySources(propertySources);
		} catch (IOException ex) {
			throw new BeanInitializationException("Could not load properties", ex);
		}

		super.postProcessBeanFactory(beanFactory);
	}
}
