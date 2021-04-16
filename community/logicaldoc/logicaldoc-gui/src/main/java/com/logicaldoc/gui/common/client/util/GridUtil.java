package com.logicaldoc.gui.common.client.util;

import java.util.Date;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;

public class GridUtil {

	/**
	 * Scrolls the grid trying to display all the record and fetch all the data
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

			ContactingServer.get().show();

			listGrid.scrollToRow(0);
			listGrid.draw();

			if (listGrid.getVisibleRows()[0] == -1)
				return;

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
							ContactingServer.get().hide();
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
		scrollGrid(listGrid, new EndScrollListener() {

			@Override
			public void endScroll(ListGrid listGrid) {
				ListGrid.showPrintPreview(listGrid);

//              Uncomment to realize the preview page by our own
//
//				ContactingServer.get().show();
//				listGrid.getPrintHTML(null, new PrintHTMLCallback() {
//
//					@Override
//					public void setHTML(String html) {
//						ContactingServer.get().hide();
//
//						String title = listGrid.getTitle() != null ? listGrid.getTitle() : I18N.message("print");
//
//						StringBuffer content = new StringBuffer("<html><head>\n");
//
//						// Prepare the header
//						content.append("<title>" + title + "</title>");
//						content.append("\n<style>\n");
//						content.append(".cell, .cellDarkAltCol, .cellDark{white-space: nowrap;}\n ");
//						content.append(
//								".printHeader{white-space: nowrap; font-weight: bold; border:0px solid white;}\n");
//						content.append("\n</style>\n");
//
//						content.append(
//								"<link href='" + Util.contextPath() + "fontawesome/css/all.css' rel='stylesheet' />\n");
//
//						content.append("<link REL='STYLESHEET' HREF='" + GWT.getModuleBaseURL() + "sc/skins/"
//								+ Util.currentSkin() + "/style.css' TYPE='text/css' />");
//
//						content.append(
//								"\n<script type='text/javascript'>function printPage(){document.getElementById('printPanel').style.display='none'; window.print(); window.close();}</script>\n");
//
//						content.append("</head><body>\n");
//
//						// Put the panel with the buttons
//						content.append(
//								"<div id='printPanel' class='printPanel default'><ul><li><a href='javascript:printPage();' id='printButton'>"
//										+ I18N.message("print")
//										+ "</a></li><li><a href='javascript:window.close();' id='printClose'>"
//										+ I18N.message("close") + "</a></li></ul></div>");
//
//						// Get the HTML of the grid(a <table>) and manipulate
//						// some details
//						String html2 = html.replaceAll("text-overflow:ellipsis;", "").replaceAll("OVERFLOW:hidden;",
//								"");
//						content.append(html2);
//
//						// Some scripts and styles at the end of the document
//						content.append("\n<link rel='stylesheet' type='text/css' href='" + Util.contextPath()
//								+ "skin/css?tenant=" + Session.get().getTenantName() + "' />\n");
//
//						content.append("\n</body>\n");
//						content.append("\n</html>");
//
//						WindowUtils.openHtmlInWindow(title, content.toString());
//					}
//				});
			}
		});
	}

	/**
	 * Exports into the CSV format the content of a ListGrid.
	 * 
	 * @param listGrid Grid containing the data
	 * @param allFields True if all the fields(even if hidden) have to be
	 *        extracted
	 */
	public static void exportCSV(ListGrid listGrid, boolean allFields) {
		scrollGrid(listGrid, new EndScrollListener() {

			@Override
			public void endScroll(ListGrid listGrid) {
				exportCSVAfterDataCollected(listGrid, allFields);
			}
		});
	}

	private static void exportCSVAfterDataCollected(ListGrid listGrid, boolean allFields) {
		try {
			StringBuilder stringBuilder = new StringBuilder(); // csv data in
																// here

			// column names
			ListGridField[] fields = listGrid.getFields();
			if (allFields)
				fields = listGrid.getAllFields();
			for (int i = 0; i < fields.length; i++) {
				ListGridField listGridField = fields[i];
				if (listGridField.getType().equals(ListGridFieldType.ICON)
						|| listGridField.getType().equals(ListGridFieldType.IMAGE)
						|| listGridField.getType().equals(ListGridFieldType.IMAGEFILE)
						|| listGridField.getType().equals(ListGridFieldType.BINARY)
						|| "".equals(listGridField.getTitle()) || "&nbsp;".equals(listGridField.getTitle()))
					continue;

				stringBuilder.append("\"");
				stringBuilder.append(listGridField.getTitle());
				stringBuilder.append("\";");
			}
			stringBuilder.deleteCharAt(stringBuilder.length() - 1); // remove
																	// last
																	// ";"
			stringBuilder.append("\n");

			// column data
			Record[] records = new Record[0];
			try {
				records = listGrid.getRecords();
			} catch (Throwable t) {
			}

			if (records == null || records.length < 1) {
				/*
				 * In case of data bound grid, we need to call the original
				 * records list
				 */
				RecordList buf = listGrid.getOriginalRecordList();
				if (buf != null) {
					records = new Record[buf.getLength()];
					for (int i = 0; i < records.length; i++)
						records[i] = buf.get(i);
				}
			}

			for (int i = 0; i < records.length; i++) {
				Record record = records[i];

				for (int j = 0; j < fields.length; j++) {
					try {
						ListGridField listGridField = fields[j];
						if (listGridField.getType().equals(ListGridFieldType.ICON)
								|| listGridField.getType().equals(ListGridFieldType.IMAGE)
								|| listGridField.getType().equals(ListGridFieldType.IMAGEFILE)
								|| listGridField.getType().equals(ListGridFieldType.BINARY)
								|| "".equals(listGridField.getTitle()) || "&nbsp;".equals(listGridField.getTitle()))
							continue;

						stringBuilder.append("\"");
						if (listGridField.getType().equals(ListGridFieldType.DATE)) {
							Date val = record.getAttributeAsDate(listGridField.getName());
							stringBuilder.append(val == null ? "" : I18N.formatDateShort(val));
						} else {
							Object val = record.getAttribute(listGridField.getName());
							stringBuilder.append(val == null || "null".equals(val.toString()) ? "" : val.toString());
						}
						stringBuilder.append("\";");
					} catch (Throwable t) {
						/*
						 * May be that not all the rows are available, since we
						 * can count just on the rows that were rendered.
						 */
					}
				}
				stringBuilder.deleteCharAt(stringBuilder.length() - 1); // remove
																		// last
																		// ";"
				stringBuilder.append("\n");
			}
			String content = stringBuilder.toString();

			/*
			 * Now post the CSV content to the server
			 */
			RequestBuilder builder = new RequestBuilder(RequestBuilder.POST,
					Util.contextPath().endsWith("/") ? Util.contextPath() + "csv" : Util.contextPath() + "/csv");
			builder.setHeader("Content-type", "application/csv");

			try {
				builder.sendRequest(content, new RequestCallback() {
					public void onError(Request request, Throwable exception) {
						GuiLog.error(exception.getMessage(), null, exception);
					}

					public void onResponseReceived(Request request, Response response) {
						/*
						 * Now we can download the complete file
						 */
						WindowUtils.openUrl(GWT.getHostPageBaseURL().endsWith("/") ? GWT.getHostPageBaseURL() + "csv"
								: GWT.getHostPageBaseURL() + "/csv");
					}
				});
			} catch (RequestException e) {
				GWT.log("error", e);
			}
		} finally {
			ContactingServer.get().hide();
		}
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
}