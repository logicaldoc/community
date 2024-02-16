package com.logicaldoc.gui.frontend.client.document;

import java.util.LinkedHashMap;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.data.RatingsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;

public class RatingDialog extends Window {

	private GUIRating rating = null;

	private ValuesManager vm = new ValuesManager();

	public RatingDialog(int documentRating, GUIRating rat) {
		super();
		this.rating = rat;

		addCloseClickHandler(event -> destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("rating"));
		setWidth(250);
		setHeight(150);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);
		setAutoSize(true);
		setAlign(Alignment.LEFT);

		final VLayout layout = new VLayout(5);
		layout.setTop(20);
		layout.setMargin(5);

		final DynamicForm ratingForm = new DynamicForm();
		ratingForm.setAlign(Alignment.LEFT);
		ratingForm.setTitleOrientation(TitleOrientation.LEFT);
		ratingForm.setNumCols(1);
		ratingForm.setValuesManager(vm);

		StaticTextItem actualRating = ItemFactory.newStaticTextItem("actualrating",
				DocUtil.getRatingIcon(documentRating));

		final StaticTextItem totalVotes = ItemFactory.newStaticTextItem("totalvotes",
				this.rating.getCount().toString() + "&nbsp;&nbsp;" + AwesomeFactory.getIconHtml("eye"));
		totalVotes.setWrapTitle(false);
		totalVotes.setWrap(false);
		totalVotes.setEndRow(true);
		totalVotes.setAlign(Alignment.LEFT);
		totalVotes.setPrompt(I18N.message("showvoters"));
		totalVotes.addClickHandler(event -> {
			ListGridField vote = new ListGridField("vote", I18N.message("vote"), 94);
			vote.setAlign(Alignment.CENTER);
			vote.setCellFormatter((value, rec, rowNum, colNum) -> DocUtil.getRatingIcon((Integer) value));

			ListGridField user = new ListGridField("user", I18N.message("user"), 140);
			ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_SHORT);

			ListGrid votesGrid = new ListGrid();
			votesGrid.setWidth100();
			votesGrid.setHeight100();
			votesGrid.setAutoFetchData(true);
			votesGrid.setDataSource(new RatingsDS(rating.getDocId()));
			votesGrid.setFields(date, user, vote);

			final Window dialog = new Window();
			dialog.setAutoCenter(true);
			dialog.setIsModal(true);
			dialog.setShowHeader(false);
			dialog.setShowEdges(false);
			dialog.setWidth(340);
			dialog.setHeight(200);
			dialog.setCanDragResize(true);
			dialog.setDismissOnEscape(true);
			dialog.addItem(votesGrid);
			dialog.addDoubleClickHandler(evt -> dialog.destroy());

			dialog.show();
		});

		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		for (int i = 1; i <= 5; i++)
			map.put("" + i, DocUtil.getRatingIcon(i));

		final SelectItem yourVote = new SelectItem("stars", I18N.message("yourvote"));
		yourVote.setWrapTitle(false);
		yourVote.setWidth(120);
		yourVote.setValueMap(map);
		yourVote.setPickListWidth(90);
		yourVote.setPickListHeight(130);

		ButtonItem vote = new ButtonItem();
		vote.setTitle(I18N.message("vote"));
		vote.setAutoFit(true);
		vote.addClickHandler(event -> {
			vm.validate();
			if (Boolean.FALSE.equals(vm.hasErrors())) {
				RatingDialog.this.rating.setUserId(Session.get().getUser().getId());
				RatingDialog.this.rating.setVote(Integer.parseInt(vm.getValueAsString("stars")));

				DocumentService.Instance.get().saveRating(RatingDialog.this.rating, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						destroy();
					}

					@Override
					public void onSuccess(Integer rating) {
						GuiLog.info(I18N.message("votesaved"), null);
						afterSaveOrDelete();
					}
				});
			}
			destroy();
		});

		ratingForm.setItems(actualRating, totalVotes, yourVote, vote);
		layout.addMember(ratingForm);

		DocumentService.Instance.get().getUserRating(rat.getDocId(), new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(final GUIRating vote) {
				if (vote != null) {
					yourVote.setValue("" + vote.getVote());

					final DynamicForm alreadyVotedForm = new DynamicForm();
					alreadyVotedForm.setAlign(Alignment.LEFT);
					alreadyVotedForm.setTitleOrientation(TitleOrientation.TOP);
					alreadyVotedForm.setNumCols(1);

					StaticTextItem alreadyVoted = ItemFactory.newStaticTextItem("alreadyVoted", "",
							"<b>" + I18N.message("alreadyvoted") + "</b>");
					alreadyVoted.setShouldSaveValue(false);
					alreadyVoted.setAlign(Alignment.LEFT);
					alreadyVoted.setTextBoxStyle("footerWarn");
					alreadyVoted.setShowTitle(false);
					alreadyVoted.setWrapTitle(false);
					alreadyVoted.setWrap(false);
					alreadyVotedForm.setItems(alreadyVoted);

					ButtonItem delete = new ButtonItem("delete", I18N.message("deleteyourvote"));
					delete.addClickHandler(event -> DocumentService.Instance.get().deleteRating(vote.getId(),
							new AsyncCallback<>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Integer rating) {
									afterSaveOrDelete();
								}
							}));

					alreadyVotedForm.setItems(alreadyVoted, delete);
					layout.addMember(alreadyVotedForm);
				}
			}
		});

		addItem(layout);
	}

	private void afterSaveOrDelete() {
		// We have to reload the document because
		// the rating is changed. We need to know if
		// this operation into the Documents list
		// panel or into the Search list panel.
		DocumentService.Instance.get().getById(rating.getDocId(), new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDocument doc) {
				DocumentController.get().modified(doc);
				destroy();
			}
		});
	}
}
