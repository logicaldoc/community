package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Dashlet specialized in listing document's history records
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentHistoryDashlet extends DocumentDashlet {

	private static final String EVENT_STR = "event";

	protected String event;

	public DocumentHistoryDashlet(GUIDashlet guiDashlet) {
		super(guiDashlet);

		setEvent(guiDashlet);

		HeaderControl markAsRead = new HeaderControl(HeaderControl.TRASH, e -> {
			LD.contactingServer();
			DocumentService.Instance.get().markHistoryAsRead(event, new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void ret) {
					LD.clearPrompt();
					RecordList l = list.getRecordList();
					for (int i = 0; i < list.getTotalRows(); i++) {
						l.get(i).setAttribute("new", false);
					}
					list.redraw();
					setTitle(I18N.message(guiDashlet.getTitle(), Integer.toString(list.getTotalRows())));
				}
			});
		});
		markAsRead.setTooltip(I18N.message("maskallasread"));

		if (event != null)
			setHeaderControls(HeaderControls.HEADER_LABEL, markAsRead, refreshControl, exportControl, printControl,
					HeaderControls.MINIMIZE_BUTTON, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
		else
			setHeaderControls(HeaderControls.HEADER_LABEL, refreshControl, exportControl, printControl,
					HeaderControls.MINIMIZE_BUTTON, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
	}

	private void setEvent(GUIDashlet guiDashlet) {
		if (guiDashlet.getName().equals("checkout"))
			event = Constants.EVENT_CHECKEDOUT;
		else if (guiDashlet.getName().equals("checkin"))
			event = Constants.EVENT_CHECKEDIN;
		else if (guiDashlet.getName().equals("locked"))
			event = Constants.EVENT_LOCKED;
		else if (guiDashlet.getName().equals("download"))
			event = Constants.EVENT_DOWNLOADED;
		else if (guiDashlet.getName().equals("change"))
			event = Constants.EVENT_CHANGED;
		else if ("lastaccessed".equals(guiDashlet.getName()))
			setTitle(AwesomeFactory.getIconHtml("eye", I18N.message(guiDashlet.getTitle())));
	}

	@Override
	public String getDocIdAttribute() {
		return "docId";
	}

	@Override
	protected RefreshableListGrid getListGrid() {
		return new DocumentsListGrid(guiDashlet.getExtendedAttributes()) {

			@Override
			protected void prepareFieldsMap() {
				super.prepareFieldsMap();

				ColoredListGridField docId = new ColoredListGridField("docId");
				docId.setHidden(true);
				fieldsMap.put(docId.getName(), docId);

				DateListGridField date = new DateListGridField("date", "date");
				date.setHidden(true);
				fieldsMap.put(date.getName(), date);

				ListGridField evnt = new ColoredListGridField(EVENT_STR);
				evnt.setHidden(true);
				fieldsMap.put(evnt.getName(), evnt);

				ListGridField path = new ColoredListGridField("path");
				path.setHidden(true);
				fieldsMap.put(path.getName(), path);

				ListGridField reason = new ColoredListGridField("reason");
				reason.setHidden(true);
				fieldsMap.put(reason.getName(), reason);

				ListGridField user = new UserListGridField("user", "userid", "user",
						Session.get().getConfigAsBoolean("gui.avatar.showingrids"));
				user.setCanFilter(true);
				user.setHidden(true);
				user.setCanSort(true);
				fieldsMap.put(user.getName(), user);

				ListGridField _new = new ListGridField("new", "new");
				_new.setHidden(true);
				fieldsMap.put(_new.getName(), _new);
			}

			@Override
			protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
				if ("true".equals(rec.getAttributeAsString("new")) && event != null) {
					return "font-weight: bold;";
				} else {
					return super.getCellCSSText(rec, rowNum, colNum);
				}
			}
		};
	}

	@Override
	protected List<ListGridField> prepareGridFields(RefreshableListGrid grid) {
		List<ListGridField> fields = super.prepareGridFields(grid);

		Map<String, ListGridField> fieldsMap = ((DocumentsListGrid) grid).getFieldsMap();

		if (!fields.contains(fieldsMap.get("id")) && fieldsMap.get("id") != null) {
			fields.add(fieldsMap.get("id"));
		}
		if (!fields.contains(fieldsMap.get("date")) && fieldsMap.get("date") != null) {
			fieldsMap.get("date").setHidden(true);
			fields.add(fieldsMap.get("date"));
		}
		if (!fields.contains(fieldsMap.get(EVENT_STR)) && fieldsMap.get(EVENT_STR) != null) {
			fieldsMap.get(EVENT_STR).setHidden(true);
			fields.add(fieldsMap.get(EVENT_STR));
		}
		if (!fields.contains(fieldsMap.get("path")) && fieldsMap.get("path") != null) {
			fieldsMap.get("path").setHidden(true);
			fields.add(fieldsMap.get("path"));
		}

		// Count the total of events and the total of unchecked events
		grid.addDataArrivedHandler(e -> onDataArrived(grid));

		return fields;
	}

	private void onDataArrived(RefreshableListGrid grid) {
		String title = I18N.message(guiDashlet.getTitle());
		String icn = "file";
		if ("lastaccessed".equals(guiDashlet.getName()))
			icn = "eye";

		if (event != null) {
			Record[] records = grid.getRecordList().toArray();
			int unread = 0;
			for (Record rec : records) {
				if ("true".equals(rec.getAttributeAsString("new")))
					unread++;
			}

			int total = grid.getTotalRows();
			title = I18N.message(event + "docs", Integer.toString(total));
			if (unread > 0)
				title = "<b>" + title + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" + I18N.message("newitems") + ": "
						+ unread + "</b>";

			if (event.equals(Constants.EVENT_CHECKEDOUT))
				icn = "edit";
			else if (event.equals(Constants.EVENT_LOCKED))
				icn = "lock-alt";
			else if (event.equals(Constants.EVENT_DOWNLOADED))
				icn = "download";
			else if (event.equals(Constants.EVENT_CHECKEDIN))
				icn = "check-square";
			else if (event.equals(Constants.EVENT_CHANGED))
				icn = "edit";

			updateStatusIconCounters(total);
		}

		setTitle(AwesomeFactory.getIconHtml(icn, title));
	}

	private void updateStatusIconCounters(int total) {
		if (Constants.EVENT_LOCKED.equals(event))
			Session.get().getUser().setLockedDocs(total);
		else if (Constants.EVENT_CHECKEDOUT.equals(event))
			Session.get().getUser().setCheckedOutDocs(total);
	}
}