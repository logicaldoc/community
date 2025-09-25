package com.logicaldoc.gui.frontend.client.document.split;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.SplitService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * Dialog to ask the user the splitting policy
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4
 */
public class SplitDialog extends Window {
	public SplitDialog(GUIDocument document) {
		addCloseClickHandler(event -> destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("split") + " - " + document.getFileName());
		setCanDragResize(true);
		setIsModal(true);
		setAutoSize(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(4);

		final DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setMargin(5);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		SelectItem separator = ItemFactory.newSplitSeparatorHandlingSelector();
		separator.setDisabled(true);

		TextItem expression = ItemFactory.newTextItem("expression", null);
		expression.setDisabled(true);

		SelectItem policy = ItemFactory.newSplittingPolicySelector();
		policy.addChangedHandler(event -> {
			int plcy = Integer.parseInt(event.getValue().toString());
			switch (plcy) {
			case SplitService.POLICY_ALLPAGES:
				separator.setDisabled(true);
				expression.setDisabled(true);
				expression.setHint("");
				break;
			case SplitService.POLICY_SELECTION:
				separator.setDisabled(true);
				expression.setDisabled(false);
				expression.setHint(I18N.message("pagesselhint"));
				break;
			case SplitService.POLICY_BLANKPAGE:
				separator.setDisabled(false);
				expression.setDisabled(true);
				expression.setHint("");
				break;
			default:
				separator.setDisabled(false);
				expression.setDisabled(false);
				expression.setHint(I18N.message("regularexpression"));
			}
		});

		ButtonItem split = new ButtonItem();
		split.setTitle(I18N.message("split"));
		split.setAutoFit(true);

		split.addClickHandler(event -> {
			if (form.validate()) {
				LD.contactingServer();
				SplitService.Instance.get().split(document.getId(),
						Integer.parseInt(form.getValueAsString("splittingpolicy")),
						Integer.parseInt(form.getValueAsString("separatorhandling")),
						form.getValueAsString("expression"), new DefaultAsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								LD.clearPrompt();
								super.onFailure(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								LD.clearPrompt();
								destroy();
							}
						});
			}
		});

		form.setFields(policy, expression, separator, split);
		addItem(form);
	}
}
