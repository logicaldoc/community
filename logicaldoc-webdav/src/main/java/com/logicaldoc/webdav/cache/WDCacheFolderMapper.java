package com.logicaldoc.webdav.cache;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

public class WDCacheFolderMapper implements RowMapper<WDCacheFolder> {
    @Override
    public WDCacheFolder mapRow(ResultSet rs, int rowNum) throws SQLException {
    	WDCacheFolder wcf = new WDCacheFolder();

    	wcf.id = rs.getLong("ID");
    	wcf.path = rs.getString("path");
    	wcf.size = rs.getLong("size");

        return wcf;
    }
}
