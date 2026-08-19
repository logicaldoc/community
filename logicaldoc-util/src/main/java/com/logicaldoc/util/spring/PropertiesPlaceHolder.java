package com.logicaldoc.util.spring;

import java.util.Properties;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertiesPropertySource;
import org.springframework.core.env.PropertySource;

import com.logicaldoc.util.config.ContextProperties;

/**
 * Extends the standard property placeholder to retrieve properties from the
 * database.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
public class PropertiesPlaceHolder extends PropertySourcesPlaceholderConfigurer {

    @Override
    public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
        MutablePropertySources propertySources = new MutablePropertySources();
        try {
            ContextProperties config = new ContextProperties();

            /*
             * Create a standard Properties with the replacements and decodings
             * already done by our ContextProperties, this way the
             * PropertiesPropertySource will be able to gather the real value
             * for each setting
             */
            Properties props = new Properties();
            for (String name : config.getKeys())
                props.setProperty(name, config.getString(name));

            PropertySource<?> localPropertySource = new PropertiesPropertySource(LOCAL_PROPERTIES_PROPERTY_SOURCE_NAME,
                    props);

            if (this.localOverride) {
                propertySources.addFirst(localPropertySource);
                propertySources.addLast(new DatabasePropertySource());
            } else {
                propertySources.addFirst(new DatabasePropertySource());
                propertySources.addLast(localPropertySource);
            }

            setPropertySources(propertySources);
        } catch (Exception ex) {
            throw new BeanInitializationException("Could not load properties", ex);
        }

        super.postProcessBeanFactory(beanFactory);
    }
}
