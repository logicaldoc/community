package com.logicaldoc.webdav.cache;

import java.sql.SQLException;

import javax.sql.DataSource;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

import com.logicaldoc.core.folder.Folder;

/**
 * A cache for the WebDAV module
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 9.1.1
 */
public class WebdavCache {

	private static final Logger log = LoggerFactory.getLogger(WebdavCache.class);

	private DataSource dataSource;

	private JdbcTemplate jdbcTemplate;

	private static WebdavCache myself;

	public WebdavCache() throws SQLException {
		dataSource = createDataSource();
		jdbcTemplate = getTemplate(dataSource);
	}

	public static WebdavCache getInstance() {
		if (myself == null)
			try {
				myself = new WebdavCache();
			} catch (SQLException e) {
				log.error(e.getMessage(), e);
			}
		return myself;
	}

	private DataSource createDataSource() {
		return new EmbeddedDatabaseBuilder().setType(EmbeddedDatabaseType.HSQL).addScript("classpath:WDCache.sql")
				.build();
	}

	private JdbcTemplate getTemplate(DataSource dataSource) {
		return new JdbcTemplate(dataSource);
	}

	public int addFolder(WebdavCacheFolder cf) {
		int result = jdbcTemplate.update("UPDATE mycache SET path = ?, size = ? WHERE ID = ?", cf.getId(), cf.getPath(),
				cf.getSize());
		if (result == 0)
			result = jdbcTemplate.update("INSERT INTO mycache VALUES (?, ?, ?)", cf.getId(), cf.getPath(),
					cf.getSize());
		return result;
	}

	public WebdavCacheFolder getFolder(long id) {
		String query = "SELECT * FROM mycache WHERE ID = ?";
		return jdbcTemplate.queryForObject(query, (rowSet, rowNum) -> new WebdavCacheFolder(rowSet.getLong("ID"),
				rowSet.getString("path"), rowSet.getLong("size")), id);
	}

	public long getFolderSize(long id) {
		try {
			return jdbcTemplate != null
					? jdbcTemplate.queryForObject("SELECT size FROM mycache WHERE ID = " + id, Long.class)
					: 0L;
		} catch (NullPointerException | DataAccessException e) {
			return 0L;
		}
	}

	public long getTreeSize(long id) {
		try {
			return jdbcTemplate != null
					? jdbcTemplate.queryForObject("SELECT SUM(size) FROM mycache WHERE path LIKE '%" + id + "%'",
							Long.class)
					: 0L;
		} catch (NullPointerException | DataAccessException e) {
			return 0;
		}
	}

	public int countElements() {
		try {
			return jdbcTemplate != null ? jdbcTemplate.queryForObject("SELECT COUNT(*) FROM mycache", Integer.class)
					: 0;
		} catch (NullPointerException | DataAccessException e) {
			return 0;
		}
	}

	public int setFolderPath(Folder folder) {
		if (folder != null && !StringUtils.isEmpty(folder.getPath())) {
			int result = jdbcTemplate.update("UPDATE mycache SET path = ? WHERE ID = ?", folder.getPath(),
					folder.getId());
			if (result == 0) {
				result = jdbcTemplate.update("INSERT INTO mycache VALUES (?, ?, ?)", folder.getId(), folder.getPath(),
						0);
			}
			return result;
		} else {
			return 0;
		}
	}

	public String getFolderPath(long folderID) {
		try {
			return jdbcTemplate.queryForObject("SELECT path FROM mycache WHERE ID = " + folderID, String.class);
		} catch (NullPointerException | DataAccessException e) {
			return null;
		}
	}

	public int truncate() {
		return jdbcTemplate.update("DELETE FROM mycache");
	}
}