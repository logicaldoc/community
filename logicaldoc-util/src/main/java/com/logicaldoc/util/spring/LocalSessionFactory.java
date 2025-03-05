package com.logicaldoc.util.spring;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.orm.hibernate5.LocalSessionFactoryBean;

/**
 * Our own implementation of a {@link LocalSessionFactoryBean} that lookup for
 * all the annotated classes inside the root package com.logicaldoc at any level
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2
 */
public class LocalSessionFactory extends LocalSessionFactoryBean {

	private static final Logger log = LoggerFactory.getLogger(LocalSessionFactory.class);

	@Override
	public void afterPropertiesSet() throws IOException {
		// Prepare the list of all those non-abstract classes annotated with
		// Entity
		List<Class<?>> annotatedClasses = new ArrayList<>();

		/*
		 * Scan the classpath to add all the @AutomationDictionary classes
		 */
		ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
		scanner.addIncludeFilter(new AnnotationTypeFilter(Entity.class));
		for (BeanDefinition bd : scanner.findCandidateComponents("com.logicaldoc")) {
			if (bd.isAbstract())
				continue;

			try {
				annotatedClasses.add(Class.forName(bd.getBeanClassName()));
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}

		setAnnotatedClasses(annotatedClasses.toArray(new Class[0]));

		// Invoke the standard handler
		super.afterPropertiesSet();
	}
}