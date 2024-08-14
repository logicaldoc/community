package com.logicaldoc.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.ContextStartedEvent;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.util.CollectionUtils;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.event.SystemEventListener;
import com.logicaldoc.util.event.SystemEventStatus;

import net.sf.ehcache.CacheManager;

/**
 * Utility class collecting static methods related to spring's context.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class Context implements ApplicationContextAware, ApplicationListener<ApplicationEvent> {

	protected static Logger log = LoggerFactory.getLogger(Context.class);

	private static Map<SystemEventStatus, LinkedList<SystemEventListener>> systemEvents = new EnumMap<>(
			SystemEventStatus.class);

	// Singleton instance
	private static Context instance;

	// The Spring's application context
	private ApplicationContext applicationContext;

	private Context() {
		instance = this;
	}

	public static Context get() {
		return instance;
	}

	/**
	 * Gets the registry with all the configuration properties for this context
	 * 
	 * @return the instance of ContextProperties in the application context
	 */
	public ContextProperties getProperties() {
		return (ContextProperties) getBean(ContextProperties.class);
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}

	/**
	 * Retrieves a bean registered in the Spring context.
	 * 
	 * @param id The bean identifier
	 * 
	 * @return The bean instance
	 */
	public Object getBean(String id) {
		// If not found with give ID try to lowercase the first char
		return applicationContext.containsBean(id) ? applicationContext.getBean(id)
				: applicationContext.getBean(Character.toLowerCase(id.charAt(0)) + id.substring(1));
	}

	/**
	 * Retrieves the collection of all the bean ids
	 * 
	 * @return the collection of ids of all the beans available in the
	 *         application context
	 */
	public List<String> getBeanIds() {
		return Arrays.asList(applicationContext.getBeanDefinitionNames());
	}

	/**
	 * Retrieves the list of bean of the same type
	 * 
	 * @param clazz class to use as filter
	 * 
	 * @return the collection of bean instances
	 */
	public List<Object> getBeansOfType(@SuppressWarnings("rawtypes")
	Class clazz) {
		List<Object> beans = new ArrayList<>();
		for (String name : applicationContext.getBeanNamesForType(clazz)) {
			beans.add(getBean(name));
		}
		return beans;
	}

	/**
	 * Retrieves a bean registered in the Spring context using a class name. At
	 * first the fully qualified class name is checked, then if nothing was
	 * found the simple class name is used as bean id.
	 * 
	 * @param clazz The bean identifier as class name
	 * 
	 * @return The bean instance
	 */
	public Object getBean(@SuppressWarnings("rawtypes")
	Class clazz) {
		String id = clazz.getName();

		if (!applicationContext.containsBean(id))
			id = id.substring(id.lastIndexOf('.') + 1);

		return getBean(id);
	}

	/**
	 * Reloads the Spring application context.
	 * <p>
	 * NOTE: use carefully, invoke only during setup when the config.xml is
	 * changed
	 */
	public void refresh() {
		if (applicationContext != null) {
			try {
				closeCaches();
				((AbstractApplicationContext) applicationContext).stop();
				((AbstractApplicationContext) applicationContext).start();
				((AbstractApplicationContext) applicationContext).refresh();
			} catch (Exception e) {
				// Nothing to do
			}
		}
	}

	/**
	 * Takes care of shutting down all cache managers
	 */
	private static void closeCaches() {
		CacheManager cm1 = CacheManager.getInstance();
		cm1.shutdown();

		for (Object cm : Context.get().getBeansOfType(CacheManager.class)) {
			((CacheManager) cm).shutdown();
		}
	}

	/**
	 * Adds an Listener to a particular Event given from
	 * 
	 * @see SystemEventListener#getSystemStatus()
	 * 
	 * @param evt the listener
	 */
	public static void addListener(SystemEventListener evt) {

		synchronized (systemEvents) {
			LinkedList<SystemEventListener> evts = Context.systemEvents.get(evt.getSystemStatus());

			// firstly we have to initialize every systemstatus
			if (evts == null) {
				evts = new LinkedList<>();
				Context.systemEvents.put(evt.getSystemStatus(), evts);
			}

			evts.add(evt);
		}
	}

	/**
	 * Removes a particular Listener from the list
	 * 
	 * @param evt the system event
	 */
	public static void removeListener(SystemEventListener evt) {
		synchronized (systemEvents) {
			LinkedList<SystemEventListener> evts = Context.systemEvents.get(evt.getSystemStatus());
			if (CollectionUtils.isEmpty(evts))
				return;

			evts.remove(evt);
		}
	}

	public Resource[] getResources(String resourcePattern) {
		try {
			return applicationContext.getResources(resourcePattern);
		} catch (IOException e) {
			// Nothing to do
		}

		return new Resource[0];
	}

	/**
	 * Returns a Spring-Resource
	 * 
	 * @param resourceLocation name of the resource to retrieve
	 * 
	 * @return the resource instance
	 */
	public Resource getResource(String resourceLocation) {
		return applicationContext.getResource(resourceLocation);
	}

	/**
	 * Processes a newly incoming event on appropriated events that registered
	 * itself on it
	 * 
	 * @param event the event to process
	 */
	@Override
	public synchronized void onApplicationEvent(ApplicationEvent event) {
		if ((event instanceof ContextStartedEvent) || (event instanceof ContextRefreshedEvent)) {
			processEvents(SystemEventStatus.BEANS_AVAILABLE);
		} else if (event instanceof ContextClosedEvent) {
			processEvents(SystemEventStatus.SYSTEM_DESTORY);
		}
	}

	private void processEvents(SystemEventStatus st) {
		LinkedList<SystemEventListener> evts = Context.systemEvents.get(st);

		// surely its possible that no one event have been
		// registered to the current spring event
		if (CollectionUtils.isEmpty(evts))
			return;

		for (SystemEventListener evt : evts) {

			try {
				evt.processEvent();
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			}
		}
	}

	/**
	 * Closes this context
	 */
	public void close() {
		if ((applicationContext instanceof org.springframework.context.ConfigurableApplicationContext configurableContext))
			configurableContext.close();
	}
}