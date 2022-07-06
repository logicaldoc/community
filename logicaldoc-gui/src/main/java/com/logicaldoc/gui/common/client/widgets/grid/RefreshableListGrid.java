package com.logicaldoc.gui.common.client.widgets.grid;

import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.widgets.grid.ListGrid;

/**
 * ListGrid that makes it easy to refresh 
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.4
 */
public class RefreshableListGrid extends ListGrid {

	public RefreshableListGrid() {
		super();
	}

	public RefreshableListGrid(DataSource dataSource) {
		super(dataSource);
	}

	public RefreshableListGrid(JavaScriptObject jsObj) {
		super(jsObj);
	}

	
	public void refresh(DataSource newDataSource){
		if(getDataSource()!=null){
			getDataSource().destroy();
			invalidateCache();
		}
		
		setDataSource(newDataSource, getAllFields());
		fetchData();
	}
}
