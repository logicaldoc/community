package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.List;

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

	public static final int MODE_LIST = 0;

	public static final int MODE_GALLERY = 1;

	/**
	 * Updates the visualization of the proper document element
	 * 
	 * @param document the document to update
	 */
	public void updateDocument(GUIDocument document);

	/**
	 * Forces the records in the grid
	 * 
	 * @param documents collectiomn of documents to render
	 */
	public void setDocuments(List<GUIDocument> documents);

	/**
	 * Retrieves all the documents dispalyed in the grid
	 * 
	 * @return the list of documents
	 */
	public List<GUIDocument> getDocuments();

	/**
	 * Gets a bean representation of the currently selected item (not all
	 * properties are populated)
	 * 
	 * @return the first selected document
	 */
	public GUIDocument getSelectedDocument();

	/**
	 * Retrieves the index of the currently selected rec
	 * 
	 * @return the row of the selected element
	 */
	public int getSelectedIndex();

	/**
	 * Gets a bean representation of the currently selected items (not all
	 * properties are populated)
	 * 
	 * @return the documents selected in the grid
	 */
	public List<GUIDocument> getSelectedDocuments();

	/**
	 * Retrieves the list of all selected document IDs
	 * 
	 * @return identifiers of the selected documents
	 */
	public List<Long> getSelectedIds();

	/**
	 * Retrieves the list of all the ids
	 * 
	 * @return list of identifiers
	 */
	public List<Long> getIds();

	/**
	 * Clear the actual selection
	 */
	public void deselectAll();

	/**
	 * Enable the records expansion
	 */
	public void setCanExpandRows();

	/**
	 * Counts the total number of elements
	 * 
	 * @return total number of records
	 */
	public int getCount();

	/**
	 * Counts the total number of selected elements
	 * 
	 * @return number of selected rows
	 */
	public int getSelectedCount();

	/**
	 * Shows or hide the filters
	 * 
	 * @param showFilters if the filters must be displayed
	 */
	public void showFilters(boolean showFilters);

	/**
	 * Selects the specified document
	 * 
	 * @param docId identifier of the document to display
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
	 * Makes the grid support the drag
	 * 
	 * @param drag if the drag must be supported
	 */
	public void setCanDrag(boolean drag);

	/**
	 * Defines the cursor widget this grid will have to interact to
	 * 
	 * @param cursor the cursor
	 */
	public void setGridCursor(Cursor cursor);

	public Cursor getGridCursor();

	/**
	 * Defines the handler for double clicks. Implementations must check if the
	 * document is password protected
	 * 
	 * @param handler invoked when the user does the double-click
	 */
	public void registerDoubleClickHandler(DoubleClickHandler handler);

	/**
	 * Defines the handler for double clicks. Implementations must check if the
	 * document is password protected
	 * 
	 * @param handler invoked when the user changes the selection
	 */
	public void registerSelectionChangedHandler(SelectionChangedHandler handler);

	/**
	 * Defines the handler of the context menu. Implementations must check if
	 * the document is password protected
	 * 
	 * @param handler invoked when the user clicks with right-button
	 */
	public void registerCellContextClickHandler(CellContextClickHandler handler);

	public void registerDataArrivedHandler(DataArrivedHandler handler);

	public GUIFolder getFolder();

	public void destroy();

	/**
	 * Replace the actual data with a new source
	 * 
	 * @param ds the data source for the grid
	 */
	public void fetchNewData(DocumentsDS ds);

	/**
	 * Loads the layout of the grid(columns, ordering ...)
	 * 
	 * @param folder Optional folder to take the layout from
	 * 
	 * @return the page size
	 */
	public int loadGridLayout(GUIFolder folder);
}