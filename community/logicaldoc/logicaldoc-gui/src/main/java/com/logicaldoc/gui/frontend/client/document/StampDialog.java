package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to apply a stamp
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 7.3
 */
public class StampDialog extends Window {

	private DynamicForm form = new DynamicForm();

	private SelectItem stamp;

	public StampDialog(final long[] ids) {
		VLayout layout = new VLayout();
		layout.setMargin(5);

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("applystamp"));
		setWidth(420);
		setHeight(115);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		layout.addMember(form);

		stamp = ItemFactory.newStampSelector();
		stamp.setTitle(I18N.message("choosestamp"));
		stamp.setWrapTitle(false);
		stamp.setRequired(true);

		ButtonItem apply = new ButtonItem();
		apply.setTitle(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onApply(ids);
			}
		});

		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(stamp, apply);
		addItem(layout);
	}

	public void onApply(long[] ids) {
		if (!form.validate())
			return;

		ContactingServer.get().show();
		ListGridRecord selection = stamp.getSelectedRecord();
		StampService.Instance.get().applyStamp(ids, Long.parseLong(selection.getAttributeAsString("id")),
				new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						ContactingServer.get().hide();
						Log.info(I18N.message("event.stamped"), null);
						destroy();
					}
				});
	}
}