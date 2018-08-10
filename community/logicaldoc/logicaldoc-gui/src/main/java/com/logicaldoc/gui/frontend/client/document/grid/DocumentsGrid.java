package com.logicaldoc.gui.frontend.client.document.grid;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;

/**
 * Shows a view on a collecion of documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 7.0
 */
public interface DocumentsGrid {

	public final static int MODE_LIST = 0;

	public final static int MODE_GALLERY = 1;

	/**
	 * Updates the visualization of the proper document element
	 */
	public void updateDocument(GUIDocument document);

	/**
	 * Forces the records in the grid
	 */
	public void setDocuments(GUIDocument[] documents);

	/**
	 * Retrieves all the documents
	 */
	public GUIDocument[] getDocuments();

	/**
	 * Gets a bean representation of the currently selected item (not all
	 * properties are populated).
	 */
	public GUIDocument getSelectedDocument();

	/**
	 * Retrieves the index of the currently selected record
	 */
	public int getSelectedIndex();

	/**
	 * Gets a bean representation of the currently selected items (not all
	 * properties are populated).
	 */
	public GUIDocument[] getSelectedDocuments();

	/**
	 * Retrieves the list of all selected documents
	 */
	public long[] getSelectedIds();

	/**
	 * Retrieves the list of all the ids
	 */
	public long[] getIds();

	/**
	 * Deselect all documents
	 */
	public void deselectAll();

	/**
	 * Enabled the records expansion
	 */
	public void setCanExpandRows();

	/**
	 * Counts the total number of elements
	 */
	public int getCount();

	/**
	 * Counts the total number of selected elements
	 */
	public int getSelectedCount();

	/**
	 * Shows or hide the filters
	 */
	public void showFilters(boolean showFilters);

	/**
	 * Selects the specified document
	 */
	public void selectDocument(long docId);

	/**
	 * Removed the selected documents from visualization
	 */
	public void removeSelectedDocuments();

	/**
	 * Expands all visible rows
	 */
	public void expandVisibleRows();

	/**
	 * Tells if the grid must support the drag
	 */
	public void setCanDrag(boolean drag);

	/**
	 * Defines the cursor widget this grid will have to interact to
	 */
	public void setGridCursor(Cursor cursor);
	
	public Cursor getGridCursor();

	/**
	 * Defines the handler for double clicks. Implementations must check if the
	 * document is password protected.
	 */
	public void registerDoubleClickHandler(DoubleClickHandler handler);

	/**
	 * Defines the handler for double clicks. Implementations must check if the
	 * document is password protected.
	 */
	public void registerSelectionChangedHandler(SelectionChangedHandler handler);

	/**
	 * Defines the handler of the context menu. Implementations must check if
	 * the document is password protected.
	 */
	public void registerCellContextClickHandler(CellContextClickHandler handler);

	public void registerDataArrivedHandler(DataArrivedHandler handler);

	public GUIFolder getFolder();
	
	public void destroy();
	
	
	/**
	 * Replace the actual data with a new source.
	 */
	public void fetchNewData(DocumentsDS ds);
}