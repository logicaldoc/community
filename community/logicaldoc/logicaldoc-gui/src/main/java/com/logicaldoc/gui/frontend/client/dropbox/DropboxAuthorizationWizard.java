package com.logicaldoc.gui.frontend.client.dropbox;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.services.DropboxService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to guide the user in authorize the LogicalDOC
 * application
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class DropboxAuthorizationWizard extends Window {

	private SubmitItem submit;

	private ValuesManager vm = new ValuesManager();

	private VLayout layout = new VLayout();

	public DropboxAuthorizationWizard(String authorizationUrl) {
		layout.setMargin(25);
		layout.setMembersMargin(2);
		layout.setWidth100();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				destroy();
			}
		});

		setTitle(I18N.message("authorizelogicaldoc", Session.get().getInfo().getBranding().getProduct()));
		setWidth100();
		setHeight(180);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setTop(0);

		HTMLFlow message = new HTMLFlow(I18N.message("authorizelogicaldochint", new String[] {
				Session.get().getInfo().getBranding().getProduct(), authorizationUrl, authorizationUrl }));
		message.setWidth100();

		DynamicForm form = new DynamicForm();
		form.setMargin(3);
		form.setValuesManager(vm);

		TextItem code = ItemFactory.newTextItem("code", "authorizationcode", null);
		code.setRequired(true);
		code.setWidth(350);

		submit = new SubmitItem();
		submit.setTitle(I18N.message("submit"));
		submit.setAlign(Alignment.RIGHT);
		submit.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSubmit();
			}
		});

		form.setItems(code, submit);

		layout.setMembers(message, form);
		addItem(layout);

		// Invoke the Dropbox authorization web page
		String prefs = "top=" + (Integer.parseInt(WindowUtils.top()) + 350) + ", left=" + WindowUtils.left()
				+ ", width=" + com.google.gwt.user.client.Window.getClientWidth() + ", height=450"
				+ ", toolbar=no, scrollbars=yes, resizable=yes";
		WindowUtils.openUrl(authorizationUrl, "_blank", prefs);
		WindowUtils.focus();
	}

	public void onSubmit() {
		if (!vm.validate())
			return;

		DropboxService.Instance.get().finishAuthorization(vm.getValueAsString("code").trim(), new AsyncCallback<String>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(String account) {
				if (account != null) {
					destroy();
					SC.say(I18N.message("correctlyauthorized", new String[] { Session.get().getInfo().getBranding().getProductName(),
							account }));
				} else
					SC.warn(I18N.message("unabletoauthorize"));
			}
		});
	}
}