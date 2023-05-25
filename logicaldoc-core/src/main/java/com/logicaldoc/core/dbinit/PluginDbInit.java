package com.logicaldoc.core.dbinit;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.dbinit.DBInit;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * Database initialization manager
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class PluginDbInit extends DBInit {

	protected static Logger log = LoggerFactory.getLogger(PluginDbInit.class);

	/**
	 * Intitializes the database using 'DbInit' extension point.
	 * 
	 * @throws SQLException Error in the database
	 */
	public void init() throws SQLException {
		init(null);
	}

	public void init(String[] ids) throws SQLException {
		log.info("Start database initialization");
		log.info("Database engine is " + getDbms());

		try {
			// Acquire the 'DbInit' extensions of the core plugin
			PluginRegistry registry = PluginRegistry.getInstance();
			Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "DbInit");

			// Sort the extensions according to ascending position
			List<Extension> sortedExts = new ArrayList<>();
			for (Extension extension : exts) {
				sortedExts.add(extension);
			}
			Collections.sort(sortedExts, (Extension e1, Extension e2) -> {
				int position1 = Integer.parseInt(e1.getParameter("position").valueAsString());
				int position2 = Integer.parseInt(e2.getParameter("position").valueAsString());
				if (position1 < position2)
					return -1;
				else if (position1 > position2)
					return 1;
				else
					return 0;
			});

			getSqlList().clear();

			List<String> idSet = new ArrayList<>();
			if (ids != null)
				idSet = Arrays.asList(ids);

			// Acquire the ordered list of sql files
			for (Extension ext : sortedExts) {
				String id = ext.getDeclaringPluginDescriptor().getId();
				if (!idSet.isEmpty() && !idSet.contains(id))
					continue;

				String sqlFile = ext.getParameter("sqlFile").valueAsString();
				if (exists(sqlFile + "." + getDbms().toLowerCase()))
					getSqlList().add(sqlFile + "." + getDbms().toLowerCase());
				else
					getSqlList().add(sqlFile);
			}
			execute();
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw new SQLException(e.getMessage());
		}
	}

	/**
	 * Checks resource existence into the classpath
	 */
	private boolean exists(String name) {
		ClassLoader cl = this.getClass().getClassLoader();
		try {
			return cl.getResourceAsStream(name) != null;
		} catch (Exception t) {
			return false;
		}
	}
}