package com.logicaldoc.gui.common.client.util;

import java.util.Date;

import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;

public class GridUtil {

	/**
	 * Scrolls the grid trying to display all the rec and fetch all the data
	 * 
	 * @param listGrid the grid to process
	 * @param listener optional listener inoked at the end of the scroll
	 */
	public static void scrollGrid(ListGrid listGrid, EndScrollListener listener) {
		if (listGrid.getTotalRows() > 0) {
			final int selectedRow;
			if (listGrid.getSelectedRecord() != null)
				selectedRow = listGrid.getRowNum(listGrid.getSelectedRecord());
			else
				selectedRow = 0;

			LD.contactingServer();

			listGrid.scrollToRow(0);
			listGrid.draw();

			if (listGrid.getVisibleRows()[0] == -1) {
				LD.clearPrompt();
				return;
			}

			/*
			 * With a timer we scroll the grid in order to fetch all the data
			 */
			final Timer timer = new Timer() {
				public void run() {
					Integer[] visibleRows = listGrid.getVisibleRows();
					if (visibleRows[1] >= listGrid.getTotalRows() - 1) {
						try {
							listGrid.scrollToRow(selectedRow);
							if (listener != null)
								listener.endScroll(listGrid);
						} finally {
							LD.clearPrompt();
						}
					} else if (visibleRows[0] != -1 && visibleRows[1] < listGrid.getTotalRows() - 1) {
						listGrid.scrollToRow(visibleRows[1] + 1);
						schedule(100);
					}
				}
			};
			timer.schedule(100);
		}
	}
	
	/**
	 * Scrolls the grid all down
	 * 
	 * @param listGrid the grid to process
	 * @param listener optional listener inoked at the end of the scroll
	 */
	public static void scrollDownGrid(ListGrid listGrid, EndScrollListener listener) {
		if (listGrid.getTotalRows() > 0) {
			LD.contactingServer();

			listGrid.scrollToRow(0);
			listGrid.draw();

			if (listGrid.getVisibleRows()[0] == -1) {
				LD.clearPrompt();
				return;
			}

			/*
			 * With a timer we scroll the grid in order to fetch all the data
			 */
			final Timer timer = new Timer() {
				public void run() {
					Integer[] visibleRows = listGrid.getVisibleRows();
					if (visibleRows[1] >= listGrid.getTotalRows() - 1) {
						try {
							if (listener != null)
								listener.endScroll(listGrid);
						} finally {
							LD.clearPrompt();
						}
					} else if (visibleRows[0] != -1 && visibleRows[1] < listGrid.getTotalRows() - 1) {
						listGrid.scrollToRow(visibleRows[1] + 1);
						schedule(100);
					}
				}
			};
			timer.schedule(100);
		}
	}

	/**
	 * Prints a grid
	 * 
	 * @param listGrid Grid containing the data
	 */
	public static void print(ListGrid listGrid) {
		scrollGrid(listGrid, Canvas::showPrintPreview);
	}

	/**
	 * Exports into the CSV format the content of a ListGrid.
	 * 
	 * @param listGrid Grid containing the data
	 * @param allFields True if all the fields(even if hidden) have to be
	 *        extracted
	 */
	public static void exportCSV(ListGrid listGrid, boolean allFields) {
		scrollGrid(listGrid, lg -> exportCSVAfterDataCollected(lg, allFields));

	}

	private static void exportCSVAfterDataCollected(ListGrid listGrid, boolean allFields) {
		try {
			StringBuilder stringBuilder = new StringBuilder(); // csv data in
																// here

			// Headers
			ListGridField[] fields = printColumnHeadersRow(listGrid, allFields, stringBuilder);

			// Data
			Record[] records = getData(listGrid);

			for (int i = 0; i < records.length; i++) {
				Record rec = records[i];
				printRecord(stringBuilder, fields, rec);
			}

			String content = stringBuilder.toString();

			GridUtil.exportAsCSV(content);
		} finally {
			LD.clearPrompt();
		}
	}

	private static void printRecord(StringBuilder stringBuilder, ListGridField[] fields, Record rec) {
		for (int j = 0; j < fields.length; j++) {
			try {
				ListGridField listGridField = fields[j];
				if (isImageOrIcon(listGridField))
					continue;

				printRecordColumn(stringBuilder, rec, listGridField);
			} catch (Exception t) {
				/*
				 * May be that not all the rows are available, since we can
				 * count just on the rows that were rendered.
				 */
			}
		}
		stringBuilder.deleteCharAt(stringBuilder.length() - 1); // remove
																// last
																// ";"
		stringBuilder.append("\n");
	}

	private static void printRecordColumn(StringBuilder stringBuilder, Record rec, ListGridField listGridField) {
		stringBuilder.append("\"");
		if (listGridField.getType().equals(ListGridFieldType.DATE)
				|| listGridField.getType().equals(ListGridFieldType.DATETIME)) {
			Date val = rec.getAttributeAsDate(listGridField.getName());
			stringBuilder.append(val == null ? "" : I18N.formatDateLong(val));
		} else {
			Object val = rec.getAttribute(listGridField.getName());
			stringBuilder.append(val == null || "null".equals(val.toString()) ? "" : Util.encodeUTF8(val.toString()));
		}
		stringBuilder.append("\";");
	}

	private static boolean isImageOrIcon(ListGridField listGridField) {
		return listGridField.getType().equals(ListGridFieldType.ICON)
				|| listGridField.getType().equals(ListGridFieldType.IMAGE)
				|| listGridField.getType().equals(ListGridFieldType.IMAGEFILE)
				|| listGridField.getType().equals(ListGridFieldType.BINARY) || "".equals(listGridField.getTitle())
				|| "&nbsp;".equals(listGridField.getTitle());
	}

	private static Record[] getData(ListGrid listGrid) {
		Record[] records = listGrid.getRecords();
		if (records == null || records.length < 1) {
			/*
			 * In case of data bound grid, we need to call the original records
			 * list
			 */
			RecordList buf = listGrid.getOriginalRecordList();
			if (buf != null) {
				records = new Record[buf.getLength()];
				for (int i = 0; i < records.length; i++)
					records[i] = buf.get(i);
			}
		}

		if (records == null)
			records = new Record[0];

		return records;
	}

	private static ListGridField[] printColumnHeadersRow(ListGrid listGrid, boolean allFields,
			StringBuilder stringBuilder) {
		// column names
		ListGridField[] fields = listGrid.getFields();
		if (allFields)
			fields = listGrid.getAllFields();
		for (int i = 0; i < fields.length; i++) {
			ListGridField listGridField = fields[i];
			if (isImageOrIcon(listGridField))
				continue;

			stringBuilder.append("\"");
			stringBuilder.append(Util.encodeUTF8(listGridField.getTitle()));
			stringBuilder.append("\";");
		}
		stringBuilder.deleteCharAt(stringBuilder.length() - 1); // remove
																// last
																// ";"
		stringBuilder.append("\n");
		return fields;
	}

	/**
	 * Listener to inject code at the end of a grid scroll
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 8.6.1
	 *
	 */
	public interface EndScrollListener {

		/**
		 * Invoked when the list grid has been fully scrolled
		 * 
		 * @param listGrid the scrolled grid
		 */
		public void endScroll(ListGrid listGrid);
	}

	public static native void exportAsCSV(String content) /*-{
		var element = $wnd.document.createElement('a');
		element.setAttribute('href', 'data:application/csv;charset=utf-8,'
				+ encodeURIComponent(content));
		element.setAttribute('download', "export.csv");
		element.style.display = 'none';
		$wnd.document.body.appendChild(element);

		element.click();

		$wnd.document.body.removeChild(element);
	}-*/;
}