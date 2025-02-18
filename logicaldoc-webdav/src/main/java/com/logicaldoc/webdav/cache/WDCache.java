package com.logicaldoc.webdav.cache;

import java.sql.SQLException;

import javax.sql.DataSource;

import org.apache.commons.lang3.StringUtils;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

import com.logicaldoc.core.folder.Folder;

public class WDCache {
	
	private DataSource dataSource;
	private JdbcTemplate jdbcTemplate;
	private static WDCache myself;

	public WDCache() throws SQLException {
		dataSource = createDataSource();
		jdbcTemplate = getTemplate(dataSource);
	}
	
	public static WDCache getInstance() {
		if (myself == null)
			try {
				myself = new WDCache();
			} catch (SQLException e) {
				e.printStackTrace();
			}
		return myself;
	}
	
	private DataSource createDataSource() {
	    return new EmbeddedDatabaseBuilder()
	      .setType(EmbeddedDatabaseType.HSQL)
	      .addScript("classpath:WDCache.sql").build();	
	}	
	
	private JdbcTemplate getTemplate(DataSource dataSource) {
       return new JdbcTemplate(dataSource);
    }	
	
	public int addFolder(WDCacheFolder cf) {
		int result = jdbcTemplate.update("UPDATE mycache SET path = ?, size = ? WHERE ID = ?", cf.path, cf.size, cf.id);
		if (result == 0)
			result = jdbcTemplate.update("INSERT INTO mycache VALUES (?, ?, ?)", cf.id, cf.path, cf.size);
		return result;
	}
	
	public WDCacheFolder getFolder(long id) {
		String query = "SELECT * FROM mycache WHERE ID = ?";
		return jdbcTemplate.queryForObject(query, new WDCacheFolderMapper(), id);
	}
	
	public long getFolderSize(long id) {
		try {
			Long result = jdbcTemplate.queryForObject("SELECT size FROM mycache WHERE ID = " +id, Long.class);
			if (result != null)
				return result.longValue();			
		} catch (DataAccessException e) {
			return 0;
		}
		return 0;
	}
	
	public long getTreeSize(long id) {
		try {
			Long result = jdbcTemplate.queryForObject("SELECT SUM(size) FROM mycache WHERE path LIKE '%" + id+"%'", Long.class);
			if (result != null)
				return result.longValue();
		} catch (DataAccessException e) {
			return 0;
		}
		return 0;
	}	

	public int countElements() {
		try {
			return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM mycache", Integer.class);
		} catch (DataAccessException e) {
			return 0;
		}
	}

	public int setFolderPath(Folder folder) {
		if ((folder != null) && !StringUtils.isEmpty(folder.getPath())) {
			int result = jdbcTemplate.update("UPDATE mycache SET path = ? WHERE ID = ?", folder.getPath(), folder.getId());			
			if (result == 0) {
				result = jdbcTemplate.update("INSERT INTO mycache VALUES (?, ?, ?)", folder.getId(), folder.getPath(), 0);
			}
			return result;
		}
		return 0;	
	}

	public String getFolderPath(long folderID) {
		try {
			return jdbcTemplate.queryForObject("SELECT path FROM mycache WHERE ID = " + folderID, String.class);
		} catch (DataAccessException e) {
			return null;
		}
	}

	public int truncate() {
		return jdbcTemplate.update("DELETE FROM mycache");
	}	
}
