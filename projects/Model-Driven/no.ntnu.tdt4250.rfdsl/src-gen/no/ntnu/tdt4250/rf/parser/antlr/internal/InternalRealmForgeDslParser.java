package no.ntnu.tdt4250.rf.parser.antlr.internal;

import org.eclipse.xtext.*;
import org.eclipse.xtext.parser.*;
import org.eclipse.xtext.parser.impl.*;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.common.util.Enumerator;
import org.eclipse.xtext.parser.antlr.AbstractInternalAntlrParser;
import org.eclipse.xtext.parser.antlr.XtextTokenStream;
import org.eclipse.xtext.parser.antlr.XtextTokenStream.HiddenTokens;
import org.eclipse.xtext.parser.antlr.AntlrDatatypeRuleToken;
import no.ntnu.tdt4250.rf.services.RealmForgeDslGrammarAccess;



import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

@SuppressWarnings("all")
public class InternalRealmForgeDslParser extends AbstractInternalAntlrParser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "RULE_STRING", "RULE_ID", "RULE_INT", "RULE_ML_COMMENT", "RULE_SL_COMMENT", "RULE_WS", "RULE_ANY_OTHER", "'name'", "'description'", "'unit'", "'-'", "'.'", "'E'", "'e'", "'{'", "'message'", "'}'", "'TypeRacerEvent'", "'isCaseSensitive'", "'difficulty'", "'timeLimit'", "'retries'", "'sentence'", "'result'", "'QuestionEvent'", "'question'", "'options'", "','", "'text'", "'isCorrectAnswer'", "'EASY'", "'NORMAL'", "'HARD'"
    };
    public static final int RULE_STRING=4;
    public static final int RULE_SL_COMMENT=8;
    public static final int T__19=19;
    public static final int T__15=15;
    public static final int T__16=16;
    public static final int T__17=17;
    public static final int T__18=18;
    public static final int T__11=11;
    public static final int T__33=33;
    public static final int T__12=12;
    public static final int T__34=34;
    public static final int T__13=13;
    public static final int T__35=35;
    public static final int T__14=14;
    public static final int T__36=36;
    public static final int EOF=-1;
    public static final int T__30=30;
    public static final int T__31=31;
    public static final int T__32=32;
    public static final int RULE_ID=5;
    public static final int RULE_WS=9;
    public static final int RULE_ANY_OTHER=10;
    public static final int T__26=26;
    public static final int T__27=27;
    public static final int T__28=28;
    public static final int RULE_INT=6;
    public static final int T__29=29;
    public static final int T__22=22;
    public static final int RULE_ML_COMMENT=7;
    public static final int T__23=23;
    public static final int T__24=24;
    public static final int T__25=25;
    public static final int T__20=20;
    public static final int T__21=21;

    // delegates
    // delegators


        public InternalRealmForgeDslParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public InternalRealmForgeDslParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        

    public String[] getTokenNames() { return InternalRealmForgeDslParser.tokenNames; }
    public String getGrammarFileName() { return "InternalRealmForgeDsl.g"; }



     	private RealmForgeDslGrammarAccess grammarAccess;

        public InternalRealmForgeDslParser(TokenStream input, RealmForgeDslGrammarAccess grammarAccess) {
            this(input);
            this.grammarAccess = grammarAccess;
            registerRules(grammarAccess.getGrammar());
        }

        @Override
        protected String getFirstRuleName() {
        	return "EventPack";
       	}

       	@Override
       	protected RealmForgeDslGrammarAccess getGrammarAccess() {
       		return grammarAccess;
       	}




    // $ANTLR start "entryRuleEventPack"
    // InternalRealmForgeDsl.g:65:1: entryRuleEventPack returns [EObject current=null] : iv_ruleEventPack= ruleEventPack EOF ;
    public final EObject entryRuleEventPack() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleEventPack = null;


        try {
            // InternalRealmForgeDsl.g:65:50: (iv_ruleEventPack= ruleEventPack EOF )
            // InternalRealmForgeDsl.g:66:2: iv_ruleEventPack= ruleEventPack EOF
            {
             newCompositeNode(grammarAccess.getEventPackRule()); 
            pushFollow(FOLLOW_1);
            iv_ruleEventPack=ruleEventPack();

            state._fsp--;

             current =iv_ruleEventPack; 
            match(input,EOF,FOLLOW_2); 

            }

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleEventPack"


    // $ANTLR start "ruleEventPack"
    // InternalRealmForgeDsl.g:72:1: ruleEventPack returns [EObject current=null] : (otherlv_0= 'name' ( (lv_name_1_0= ruleEString ) ) otherlv_2= 'description' ( (lv_description_3_0= ruleEString ) ) otherlv_4= 'unit' ( (lv_unit_5_0= ruleEString ) ) ( (lv_events_6_0= ruleEvent ) )* ) ;
    public final EObject ruleEventPack() throws RecognitionException {
        EObject current = null;

        Token otherlv_0=null;
        Token otherlv_2=null;
        Token otherlv_4=null;
        AntlrDatatypeRuleToken lv_name_1_0 = null;

        AntlrDatatypeRuleToken lv_description_3_0 = null;

        AntlrDatatypeRuleToken lv_unit_5_0 = null;

        EObject lv_events_6_0 = null;



        	enterRule();

        try {
            // InternalRealmForgeDsl.g:78:2: ( (otherlv_0= 'name' ( (lv_name_1_0= ruleEString ) ) otherlv_2= 'description' ( (lv_description_3_0= ruleEString ) ) otherlv_4= 'unit' ( (lv_unit_5_0= ruleEString ) ) ( (lv_events_6_0= ruleEvent ) )* ) )
            // InternalRealmForgeDsl.g:79:2: (otherlv_0= 'name' ( (lv_name_1_0= ruleEString ) ) otherlv_2= 'description' ( (lv_description_3_0= ruleEString ) ) otherlv_4= 'unit' ( (lv_unit_5_0= ruleEString ) ) ( (lv_events_6_0= ruleEvent ) )* )
            {
            // InternalRealmForgeDsl.g:79:2: (otherlv_0= 'name' ( (lv_name_1_0= ruleEString ) ) otherlv_2= 'description' ( (lv_description_3_0= ruleEString ) ) otherlv_4= 'unit' ( (lv_unit_5_0= ruleEString ) ) ( (lv_events_6_0= ruleEvent ) )* )
            // InternalRealmForgeDsl.g:80:3: otherlv_0= 'name' ( (lv_name_1_0= ruleEString ) ) otherlv_2= 'description' ( (lv_description_3_0= ruleEString ) ) otherlv_4= 'unit' ( (lv_unit_5_0= ruleEString ) ) ( (lv_events_6_0= ruleEvent ) )*
            {
            otherlv_0=(Token)match(input,11,FOLLOW_3); 

            			newLeafNode(otherlv_0, grammarAccess.getEventPackAccess().getNameKeyword_0());
            		
            // InternalRealmForgeDsl.g:84:3: ( (lv_name_1_0= ruleEString ) )
            // InternalRealmForgeDsl.g:85:4: (lv_name_1_0= ruleEString )
            {
            // InternalRealmForgeDsl.g:85:4: (lv_name_1_0= ruleEString )
            // InternalRealmForgeDsl.g:86:5: lv_name_1_0= ruleEString
            {

            					newCompositeNode(grammarAccess.getEventPackAccess().getNameEStringParserRuleCall_1_0());
            				
            pushFollow(FOLLOW_4);
            lv_name_1_0=ruleEString();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getEventPackRule());
            					}
            					set(
            						current,
            						"name",
            						lv_name_1_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EString");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            otherlv_2=(Token)match(input,12,FOLLOW_3); 

            			newLeafNode(otherlv_2, grammarAccess.getEventPackAccess().getDescriptionKeyword_2());
            		
            // InternalRealmForgeDsl.g:107:3: ( (lv_description_3_0= ruleEString ) )
            // InternalRealmForgeDsl.g:108:4: (lv_description_3_0= ruleEString )
            {
            // InternalRealmForgeDsl.g:108:4: (lv_description_3_0= ruleEString )
            // InternalRealmForgeDsl.g:109:5: lv_description_3_0= ruleEString
            {

            					newCompositeNode(grammarAccess.getEventPackAccess().getDescriptionEStringParserRuleCall_3_0());
            				
            pushFollow(FOLLOW_5);
            lv_description_3_0=ruleEString();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getEventPackRule());
            					}
            					set(
            						current,
            						"description",
            						lv_description_3_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EString");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            otherlv_4=(Token)match(input,13,FOLLOW_3); 

            			newLeafNode(otherlv_4, grammarAccess.getEventPackAccess().getUnitKeyword_4());
            		
            // InternalRealmForgeDsl.g:130:3: ( (lv_unit_5_0= ruleEString ) )
            // InternalRealmForgeDsl.g:131:4: (lv_unit_5_0= ruleEString )
            {
            // InternalRealmForgeDsl.g:131:4: (lv_unit_5_0= ruleEString )
            // InternalRealmForgeDsl.g:132:5: lv_unit_5_0= ruleEString
            {

            					newCompositeNode(grammarAccess.getEventPackAccess().getUnitEStringParserRuleCall_5_0());
            				
            pushFollow(FOLLOW_6);
            lv_unit_5_0=ruleEString();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getEventPackRule());
            					}
            					set(
            						current,
            						"unit",
            						lv_unit_5_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EString");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            // InternalRealmForgeDsl.g:149:3: ( (lv_events_6_0= ruleEvent ) )*
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( (LA1_0==21||LA1_0==28) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // InternalRealmForgeDsl.g:150:4: (lv_events_6_0= ruleEvent )
            	    {
            	    // InternalRealmForgeDsl.g:150:4: (lv_events_6_0= ruleEvent )
            	    // InternalRealmForgeDsl.g:151:5: lv_events_6_0= ruleEvent
            	    {

            	    					newCompositeNode(grammarAccess.getEventPackAccess().getEventsEventParserRuleCall_6_0());
            	    				
            	    pushFollow(FOLLOW_6);
            	    lv_events_6_0=ruleEvent();

            	    state._fsp--;


            	    					if (current==null) {
            	    						current = createModelElementForParent(grammarAccess.getEventPackRule());
            	    					}
            	    					add(
            	    						current,
            	    						"events",
            	    						lv_events_6_0,
            	    						"no.ntnu.tdt4250.rf.RealmForgeDsl.Event");
            	    					afterParserOrEnumRuleCall();
            	    				

            	    }


            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);


            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleEventPack"


    // $ANTLR start "entryRuleEvent"
    // InternalRealmForgeDsl.g:172:1: entryRuleEvent returns [EObject current=null] : iv_ruleEvent= ruleEvent EOF ;
    public final EObject entryRuleEvent() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleEvent = null;


        try {
            // InternalRealmForgeDsl.g:172:46: (iv_ruleEvent= ruleEvent EOF )
            // InternalRealmForgeDsl.g:173:2: iv_ruleEvent= ruleEvent EOF
            {
             newCompositeNode(grammarAccess.getEventRule()); 
            pushFollow(FOLLOW_1);
            iv_ruleEvent=ruleEvent();

            state._fsp--;

             current =iv_ruleEvent; 
            match(input,EOF,FOLLOW_2); 

            }

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleEvent"


    // $ANTLR start "ruleEvent"
    // InternalRealmForgeDsl.g:179:1: ruleEvent returns [EObject current=null] : (this_TypeRacerEvent_0= ruleTypeRacerEvent | this_QuestionEvent_1= ruleQuestionEvent ) ;
    public final EObject ruleEvent() throws RecognitionException {
        EObject current = null;

        EObject this_TypeRacerEvent_0 = null;

        EObject this_QuestionEvent_1 = null;



        	enterRule();

        try {
            // InternalRealmForgeDsl.g:185:2: ( (this_TypeRacerEvent_0= ruleTypeRacerEvent | this_QuestionEvent_1= ruleQuestionEvent ) )
            // InternalRealmForgeDsl.g:186:2: (this_TypeRacerEvent_0= ruleTypeRacerEvent | this_QuestionEvent_1= ruleQuestionEvent )
            {
            // InternalRealmForgeDsl.g:186:2: (this_TypeRacerEvent_0= ruleTypeRacerEvent | this_QuestionEvent_1= ruleQuestionEvent )
            int alt2=2;
            int LA2_0 = input.LA(1);

            if ( (LA2_0==21) ) {
                alt2=1;
            }
            else if ( (LA2_0==28) ) {
                alt2=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }
            switch (alt2) {
                case 1 :
                    // InternalRealmForgeDsl.g:187:3: this_TypeRacerEvent_0= ruleTypeRacerEvent
                    {

                    			newCompositeNode(grammarAccess.getEventAccess().getTypeRacerEventParserRuleCall_0());
                    		
                    pushFollow(FOLLOW_2);
                    this_TypeRacerEvent_0=ruleTypeRacerEvent();

                    state._fsp--;


                    			current = this_TypeRacerEvent_0;
                    			afterParserOrEnumRuleCall();
                    		

                    }
                    break;
                case 2 :
                    // InternalRealmForgeDsl.g:196:3: this_QuestionEvent_1= ruleQuestionEvent
                    {

                    			newCompositeNode(grammarAccess.getEventAccess().getQuestionEventParserRuleCall_1());
                    		
                    pushFollow(FOLLOW_2);
                    this_QuestionEvent_1=ruleQuestionEvent();

                    state._fsp--;


                    			current = this_QuestionEvent_1;
                    			afterParserOrEnumRuleCall();
                    		

                    }
                    break;

            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleEvent"


    // $ANTLR start "entryRuleEString"
    // InternalRealmForgeDsl.g:208:1: entryRuleEString returns [String current=null] : iv_ruleEString= ruleEString EOF ;
    public final String entryRuleEString() throws RecognitionException {
        String current = null;

        AntlrDatatypeRuleToken iv_ruleEString = null;


        try {
            // InternalRealmForgeDsl.g:208:47: (iv_ruleEString= ruleEString EOF )
            // InternalRealmForgeDsl.g:209:2: iv_ruleEString= ruleEString EOF
            {
             newCompositeNode(grammarAccess.getEStringRule()); 
            pushFollow(FOLLOW_1);
            iv_ruleEString=ruleEString();

            state._fsp--;

             current =iv_ruleEString.getText(); 
            match(input,EOF,FOLLOW_2); 

            }

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleEString"


    // $ANTLR start "ruleEString"
    // InternalRealmForgeDsl.g:215:1: ruleEString returns [AntlrDatatypeRuleToken current=new AntlrDatatypeRuleToken()] : (this_STRING_0= RULE_STRING | this_ID_1= RULE_ID ) ;
    public final AntlrDatatypeRuleToken ruleEString() throws RecognitionException {
        AntlrDatatypeRuleToken current = new AntlrDatatypeRuleToken();

        Token this_STRING_0=null;
        Token this_ID_1=null;


        	enterRule();

        try {
            // InternalRealmForgeDsl.g:221:2: ( (this_STRING_0= RULE_STRING | this_ID_1= RULE_ID ) )
            // InternalRealmForgeDsl.g:222:2: (this_STRING_0= RULE_STRING | this_ID_1= RULE_ID )
            {
            // InternalRealmForgeDsl.g:222:2: (this_STRING_0= RULE_STRING | this_ID_1= RULE_ID )
            int alt3=2;
            int LA3_0 = input.LA(1);

            if ( (LA3_0==RULE_STRING) ) {
                alt3=1;
            }
            else if ( (LA3_0==RULE_ID) ) {
                alt3=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 3, 0, input);

                throw nvae;
            }
            switch (alt3) {
                case 1 :
                    // InternalRealmForgeDsl.g:223:3: this_STRING_0= RULE_STRING
                    {
                    this_STRING_0=(Token)match(input,RULE_STRING,FOLLOW_2); 

                    			current.merge(this_STRING_0);
                    		

                    			newLeafNode(this_STRING_0, grammarAccess.getEStringAccess().getSTRINGTerminalRuleCall_0());
                    		

                    }
                    break;
                case 2 :
                    // InternalRealmForgeDsl.g:231:3: this_ID_1= RULE_ID
                    {
                    this_ID_1=(Token)match(input,RULE_ID,FOLLOW_2); 

                    			current.merge(this_ID_1);
                    		

                    			newLeafNode(this_ID_1, grammarAccess.getEStringAccess().getIDTerminalRuleCall_1());
                    		

                    }
                    break;

            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleEString"


    // $ANTLR start "entryRuleEDoubleObject"
    // InternalRealmForgeDsl.g:242:1: entryRuleEDoubleObject returns [String current=null] : iv_ruleEDoubleObject= ruleEDoubleObject EOF ;
    public final String entryRuleEDoubleObject() throws RecognitionException {
        String current = null;

        AntlrDatatypeRuleToken iv_ruleEDoubleObject = null;


        try {
            // InternalRealmForgeDsl.g:242:53: (iv_ruleEDoubleObject= ruleEDoubleObject EOF )
            // InternalRealmForgeDsl.g:243:2: iv_ruleEDoubleObject= ruleEDoubleObject EOF
            {
             newCompositeNode(grammarAccess.getEDoubleObjectRule()); 
            pushFollow(FOLLOW_1);
            iv_ruleEDoubleObject=ruleEDoubleObject();

            state._fsp--;

             current =iv_ruleEDoubleObject.getText(); 
            match(input,EOF,FOLLOW_2); 

            }

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleEDoubleObject"


    // $ANTLR start "ruleEDoubleObject"
    // InternalRealmForgeDsl.g:249:1: ruleEDoubleObject returns [AntlrDatatypeRuleToken current=new AntlrDatatypeRuleToken()] : ( (kw= '-' )? (this_INT_1= RULE_INT )? kw= '.' this_INT_3= RULE_INT ( (kw= 'E' | kw= 'e' ) (kw= '-' )? this_INT_7= RULE_INT )? ) ;
    public final AntlrDatatypeRuleToken ruleEDoubleObject() throws RecognitionException {
        AntlrDatatypeRuleToken current = new AntlrDatatypeRuleToken();

        Token kw=null;
        Token this_INT_1=null;
        Token this_INT_3=null;
        Token this_INT_7=null;


        	enterRule();

        try {
            // InternalRealmForgeDsl.g:255:2: ( ( (kw= '-' )? (this_INT_1= RULE_INT )? kw= '.' this_INT_3= RULE_INT ( (kw= 'E' | kw= 'e' ) (kw= '-' )? this_INT_7= RULE_INT )? ) )
            // InternalRealmForgeDsl.g:256:2: ( (kw= '-' )? (this_INT_1= RULE_INT )? kw= '.' this_INT_3= RULE_INT ( (kw= 'E' | kw= 'e' ) (kw= '-' )? this_INT_7= RULE_INT )? )
            {
            // InternalRealmForgeDsl.g:256:2: ( (kw= '-' )? (this_INT_1= RULE_INT )? kw= '.' this_INT_3= RULE_INT ( (kw= 'E' | kw= 'e' ) (kw= '-' )? this_INT_7= RULE_INT )? )
            // InternalRealmForgeDsl.g:257:3: (kw= '-' )? (this_INT_1= RULE_INT )? kw= '.' this_INT_3= RULE_INT ( (kw= 'E' | kw= 'e' ) (kw= '-' )? this_INT_7= RULE_INT )?
            {
            // InternalRealmForgeDsl.g:257:3: (kw= '-' )?
            int alt4=2;
            int LA4_0 = input.LA(1);

            if ( (LA4_0==14) ) {
                alt4=1;
            }
            switch (alt4) {
                case 1 :
                    // InternalRealmForgeDsl.g:258:4: kw= '-'
                    {
                    kw=(Token)match(input,14,FOLLOW_7); 

                    				current.merge(kw);
                    				newLeafNode(kw, grammarAccess.getEDoubleObjectAccess().getHyphenMinusKeyword_0());
                    			

                    }
                    break;

            }

            // InternalRealmForgeDsl.g:264:3: (this_INT_1= RULE_INT )?
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( (LA5_0==RULE_INT) ) {
                alt5=1;
            }
            switch (alt5) {
                case 1 :
                    // InternalRealmForgeDsl.g:265:4: this_INT_1= RULE_INT
                    {
                    this_INT_1=(Token)match(input,RULE_INT,FOLLOW_8); 

                    				current.merge(this_INT_1);
                    			

                    				newLeafNode(this_INT_1, grammarAccess.getEDoubleObjectAccess().getINTTerminalRuleCall_1());
                    			

                    }
                    break;

            }

            kw=(Token)match(input,15,FOLLOW_9); 

            			current.merge(kw);
            			newLeafNode(kw, grammarAccess.getEDoubleObjectAccess().getFullStopKeyword_2());
            		
            this_INT_3=(Token)match(input,RULE_INT,FOLLOW_10); 

            			current.merge(this_INT_3);
            		

            			newLeafNode(this_INT_3, grammarAccess.getEDoubleObjectAccess().getINTTerminalRuleCall_3());
            		
            // InternalRealmForgeDsl.g:285:3: ( (kw= 'E' | kw= 'e' ) (kw= '-' )? this_INT_7= RULE_INT )?
            int alt8=2;
            int LA8_0 = input.LA(1);

            if ( ((LA8_0>=16 && LA8_0<=17)) ) {
                alt8=1;
            }
            switch (alt8) {
                case 1 :
                    // InternalRealmForgeDsl.g:286:4: (kw= 'E' | kw= 'e' ) (kw= '-' )? this_INT_7= RULE_INT
                    {
                    // InternalRealmForgeDsl.g:286:4: (kw= 'E' | kw= 'e' )
                    int alt6=2;
                    int LA6_0 = input.LA(1);

                    if ( (LA6_0==16) ) {
                        alt6=1;
                    }
                    else if ( (LA6_0==17) ) {
                        alt6=2;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 6, 0, input);

                        throw nvae;
                    }
                    switch (alt6) {
                        case 1 :
                            // InternalRealmForgeDsl.g:287:5: kw= 'E'
                            {
                            kw=(Token)match(input,16,FOLLOW_11); 

                            					current.merge(kw);
                            					newLeafNode(kw, grammarAccess.getEDoubleObjectAccess().getEKeyword_4_0_0());
                            				

                            }
                            break;
                        case 2 :
                            // InternalRealmForgeDsl.g:293:5: kw= 'e'
                            {
                            kw=(Token)match(input,17,FOLLOW_11); 

                            					current.merge(kw);
                            					newLeafNode(kw, grammarAccess.getEDoubleObjectAccess().getEKeyword_4_0_1());
                            				

                            }
                            break;

                    }

                    // InternalRealmForgeDsl.g:299:4: (kw= '-' )?
                    int alt7=2;
                    int LA7_0 = input.LA(1);

                    if ( (LA7_0==14) ) {
                        alt7=1;
                    }
                    switch (alt7) {
                        case 1 :
                            // InternalRealmForgeDsl.g:300:5: kw= '-'
                            {
                            kw=(Token)match(input,14,FOLLOW_9); 

                            					current.merge(kw);
                            					newLeafNode(kw, grammarAccess.getEDoubleObjectAccess().getHyphenMinusKeyword_4_1());
                            				

                            }
                            break;

                    }

                    this_INT_7=(Token)match(input,RULE_INT,FOLLOW_2); 

                    				current.merge(this_INT_7);
                    			

                    				newLeafNode(this_INT_7, grammarAccess.getEDoubleObjectAccess().getINTTerminalRuleCall_4_2());
                    			

                    }
                    break;

            }


            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleEDoubleObject"


    // $ANTLR start "entryRuleEInt"
    // InternalRealmForgeDsl.g:318:1: entryRuleEInt returns [String current=null] : iv_ruleEInt= ruleEInt EOF ;
    public final String entryRuleEInt() throws RecognitionException {
        String current = null;

        AntlrDatatypeRuleToken iv_ruleEInt = null;


        try {
            // InternalRealmForgeDsl.g:318:44: (iv_ruleEInt= ruleEInt EOF )
            // InternalRealmForgeDsl.g:319:2: iv_ruleEInt= ruleEInt EOF
            {
             newCompositeNode(grammarAccess.getEIntRule()); 
            pushFollow(FOLLOW_1);
            iv_ruleEInt=ruleEInt();

            state._fsp--;

             current =iv_ruleEInt.getText(); 
            match(input,EOF,FOLLOW_2); 

            }

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleEInt"


    // $ANTLR start "ruleEInt"
    // InternalRealmForgeDsl.g:325:1: ruleEInt returns [AntlrDatatypeRuleToken current=new AntlrDatatypeRuleToken()] : ( (kw= '-' )? this_INT_1= RULE_INT ) ;
    public final AntlrDatatypeRuleToken ruleEInt() throws RecognitionException {
        AntlrDatatypeRuleToken current = new AntlrDatatypeRuleToken();

        Token kw=null;
        Token this_INT_1=null;


        	enterRule();

        try {
            // InternalRealmForgeDsl.g:331:2: ( ( (kw= '-' )? this_INT_1= RULE_INT ) )
            // InternalRealmForgeDsl.g:332:2: ( (kw= '-' )? this_INT_1= RULE_INT )
            {
            // InternalRealmForgeDsl.g:332:2: ( (kw= '-' )? this_INT_1= RULE_INT )
            // InternalRealmForgeDsl.g:333:3: (kw= '-' )? this_INT_1= RULE_INT
            {
            // InternalRealmForgeDsl.g:333:3: (kw= '-' )?
            int alt9=2;
            int LA9_0 = input.LA(1);

            if ( (LA9_0==14) ) {
                alt9=1;
            }
            switch (alt9) {
                case 1 :
                    // InternalRealmForgeDsl.g:334:4: kw= '-'
                    {
                    kw=(Token)match(input,14,FOLLOW_9); 

                    				current.merge(kw);
                    				newLeafNode(kw, grammarAccess.getEIntAccess().getHyphenMinusKeyword_0());
                    			

                    }
                    break;

            }

            this_INT_1=(Token)match(input,RULE_INT,FOLLOW_2); 

            			current.merge(this_INT_1);
            		

            			newLeafNode(this_INT_1, grammarAccess.getEIntAccess().getINTTerminalRuleCall_1());
            		

            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleEInt"


    // $ANTLR start "entryRuleEventResult"
    // InternalRealmForgeDsl.g:351:1: entryRuleEventResult returns [EObject current=null] : iv_ruleEventResult= ruleEventResult EOF ;
    public final EObject entryRuleEventResult() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleEventResult = null;


        try {
            // InternalRealmForgeDsl.g:351:52: (iv_ruleEventResult= ruleEventResult EOF )
            // InternalRealmForgeDsl.g:352:2: iv_ruleEventResult= ruleEventResult EOF
            {
             newCompositeNode(grammarAccess.getEventResultRule()); 
            pushFollow(FOLLOW_1);
            iv_ruleEventResult=ruleEventResult();

            state._fsp--;

             current =iv_ruleEventResult; 
            match(input,EOF,FOLLOW_2); 

            }

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleEventResult"


    // $ANTLR start "ruleEventResult"
    // InternalRealmForgeDsl.g:358:1: ruleEventResult returns [EObject current=null] : ( () otherlv_1= '{' (otherlv_2= 'message' ( (lv_message_3_0= ruleEString ) ) )? otherlv_4= '}' ) ;
    public final EObject ruleEventResult() throws RecognitionException {
        EObject current = null;

        Token otherlv_1=null;
        Token otherlv_2=null;
        Token otherlv_4=null;
        AntlrDatatypeRuleToken lv_message_3_0 = null;



        	enterRule();

        try {
            // InternalRealmForgeDsl.g:364:2: ( ( () otherlv_1= '{' (otherlv_2= 'message' ( (lv_message_3_0= ruleEString ) ) )? otherlv_4= '}' ) )
            // InternalRealmForgeDsl.g:365:2: ( () otherlv_1= '{' (otherlv_2= 'message' ( (lv_message_3_0= ruleEString ) ) )? otherlv_4= '}' )
            {
            // InternalRealmForgeDsl.g:365:2: ( () otherlv_1= '{' (otherlv_2= 'message' ( (lv_message_3_0= ruleEString ) ) )? otherlv_4= '}' )
            // InternalRealmForgeDsl.g:366:3: () otherlv_1= '{' (otherlv_2= 'message' ( (lv_message_3_0= ruleEString ) ) )? otherlv_4= '}'
            {
            // InternalRealmForgeDsl.g:366:3: ()
            // InternalRealmForgeDsl.g:367:4: 
            {

            				current = forceCreateModelElement(
            					grammarAccess.getEventResultAccess().getEventResultAction_0(),
            					current);
            			

            }

            otherlv_1=(Token)match(input,18,FOLLOW_12); 

            			newLeafNode(otherlv_1, grammarAccess.getEventResultAccess().getLeftCurlyBracketKeyword_1());
            		
            // InternalRealmForgeDsl.g:377:3: (otherlv_2= 'message' ( (lv_message_3_0= ruleEString ) ) )?
            int alt10=2;
            int LA10_0 = input.LA(1);

            if ( (LA10_0==19) ) {
                alt10=1;
            }
            switch (alt10) {
                case 1 :
                    // InternalRealmForgeDsl.g:378:4: otherlv_2= 'message' ( (lv_message_3_0= ruleEString ) )
                    {
                    otherlv_2=(Token)match(input,19,FOLLOW_3); 

                    				newLeafNode(otherlv_2, grammarAccess.getEventResultAccess().getMessageKeyword_2_0());
                    			
                    // InternalRealmForgeDsl.g:382:4: ( (lv_message_3_0= ruleEString ) )
                    // InternalRealmForgeDsl.g:383:5: (lv_message_3_0= ruleEString )
                    {
                    // InternalRealmForgeDsl.g:383:5: (lv_message_3_0= ruleEString )
                    // InternalRealmForgeDsl.g:384:6: lv_message_3_0= ruleEString
                    {

                    						newCompositeNode(grammarAccess.getEventResultAccess().getMessageEStringParserRuleCall_2_1_0());
                    					
                    pushFollow(FOLLOW_13);
                    lv_message_3_0=ruleEString();

                    state._fsp--;


                    						if (current==null) {
                    							current = createModelElementForParent(grammarAccess.getEventResultRule());
                    						}
                    						set(
                    							current,
                    							"message",
                    							lv_message_3_0,
                    							"no.ntnu.tdt4250.rf.RealmForgeDsl.EString");
                    						afterParserOrEnumRuleCall();
                    					

                    }


                    }


                    }
                    break;

            }

            otherlv_4=(Token)match(input,20,FOLLOW_2); 

            			newLeafNode(otherlv_4, grammarAccess.getEventResultAccess().getRightCurlyBracketKeyword_3());
            		

            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleEventResult"


    // $ANTLR start "entryRuleTypeRacerEvent"
    // InternalRealmForgeDsl.g:410:1: entryRuleTypeRacerEvent returns [EObject current=null] : iv_ruleTypeRacerEvent= ruleTypeRacerEvent EOF ;
    public final EObject entryRuleTypeRacerEvent() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleTypeRacerEvent = null;


        try {
            // InternalRealmForgeDsl.g:410:55: (iv_ruleTypeRacerEvent= ruleTypeRacerEvent EOF )
            // InternalRealmForgeDsl.g:411:2: iv_ruleTypeRacerEvent= ruleTypeRacerEvent EOF
            {
             newCompositeNode(grammarAccess.getTypeRacerEventRule()); 
            pushFollow(FOLLOW_1);
            iv_ruleTypeRacerEvent=ruleTypeRacerEvent();

            state._fsp--;

             current =iv_ruleTypeRacerEvent; 
            match(input,EOF,FOLLOW_2); 

            }

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleTypeRacerEvent"


    // $ANTLR start "ruleTypeRacerEvent"
    // InternalRealmForgeDsl.g:417:1: ruleTypeRacerEvent returns [EObject current=null] : (otherlv_0= 'TypeRacerEvent' otherlv_1= '{' ( (lv_isCaseSensitive_2_0= 'isCaseSensitive' ) )? otherlv_3= 'difficulty' ( (lv_difficulty_4_0= ruleDifficulty ) ) (otherlv_5= 'timeLimit' ( (lv_timeLimit_6_0= ruleEDoubleObject ) ) )? otherlv_7= 'retries' ( (lv_retries_8_0= ruleEInt ) ) otherlv_9= 'sentence' ( (lv_sentence_10_0= ruleEString ) ) otherlv_11= 'result' ( (lv_result_12_0= ruleEventResult ) ) otherlv_13= '}' ) ;
    public final EObject ruleTypeRacerEvent() throws RecognitionException {
        EObject current = null;

        Token otherlv_0=null;
        Token otherlv_1=null;
        Token lv_isCaseSensitive_2_0=null;
        Token otherlv_3=null;
        Token otherlv_5=null;
        Token otherlv_7=null;
        Token otherlv_9=null;
        Token otherlv_11=null;
        Token otherlv_13=null;
        Enumerator lv_difficulty_4_0 = null;

        AntlrDatatypeRuleToken lv_timeLimit_6_0 = null;

        AntlrDatatypeRuleToken lv_retries_8_0 = null;

        AntlrDatatypeRuleToken lv_sentence_10_0 = null;

        EObject lv_result_12_0 = null;



        	enterRule();

        try {
            // InternalRealmForgeDsl.g:423:2: ( (otherlv_0= 'TypeRacerEvent' otherlv_1= '{' ( (lv_isCaseSensitive_2_0= 'isCaseSensitive' ) )? otherlv_3= 'difficulty' ( (lv_difficulty_4_0= ruleDifficulty ) ) (otherlv_5= 'timeLimit' ( (lv_timeLimit_6_0= ruleEDoubleObject ) ) )? otherlv_7= 'retries' ( (lv_retries_8_0= ruleEInt ) ) otherlv_9= 'sentence' ( (lv_sentence_10_0= ruleEString ) ) otherlv_11= 'result' ( (lv_result_12_0= ruleEventResult ) ) otherlv_13= '}' ) )
            // InternalRealmForgeDsl.g:424:2: (otherlv_0= 'TypeRacerEvent' otherlv_1= '{' ( (lv_isCaseSensitive_2_0= 'isCaseSensitive' ) )? otherlv_3= 'difficulty' ( (lv_difficulty_4_0= ruleDifficulty ) ) (otherlv_5= 'timeLimit' ( (lv_timeLimit_6_0= ruleEDoubleObject ) ) )? otherlv_7= 'retries' ( (lv_retries_8_0= ruleEInt ) ) otherlv_9= 'sentence' ( (lv_sentence_10_0= ruleEString ) ) otherlv_11= 'result' ( (lv_result_12_0= ruleEventResult ) ) otherlv_13= '}' )
            {
            // InternalRealmForgeDsl.g:424:2: (otherlv_0= 'TypeRacerEvent' otherlv_1= '{' ( (lv_isCaseSensitive_2_0= 'isCaseSensitive' ) )? otherlv_3= 'difficulty' ( (lv_difficulty_4_0= ruleDifficulty ) ) (otherlv_5= 'timeLimit' ( (lv_timeLimit_6_0= ruleEDoubleObject ) ) )? otherlv_7= 'retries' ( (lv_retries_8_0= ruleEInt ) ) otherlv_9= 'sentence' ( (lv_sentence_10_0= ruleEString ) ) otherlv_11= 'result' ( (lv_result_12_0= ruleEventResult ) ) otherlv_13= '}' )
            // InternalRealmForgeDsl.g:425:3: otherlv_0= 'TypeRacerEvent' otherlv_1= '{' ( (lv_isCaseSensitive_2_0= 'isCaseSensitive' ) )? otherlv_3= 'difficulty' ( (lv_difficulty_4_0= ruleDifficulty ) ) (otherlv_5= 'timeLimit' ( (lv_timeLimit_6_0= ruleEDoubleObject ) ) )? otherlv_7= 'retries' ( (lv_retries_8_0= ruleEInt ) ) otherlv_9= 'sentence' ( (lv_sentence_10_0= ruleEString ) ) otherlv_11= 'result' ( (lv_result_12_0= ruleEventResult ) ) otherlv_13= '}'
            {
            otherlv_0=(Token)match(input,21,FOLLOW_14); 

            			newLeafNode(otherlv_0, grammarAccess.getTypeRacerEventAccess().getTypeRacerEventKeyword_0());
            		
            otherlv_1=(Token)match(input,18,FOLLOW_15); 

            			newLeafNode(otherlv_1, grammarAccess.getTypeRacerEventAccess().getLeftCurlyBracketKeyword_1());
            		
            // InternalRealmForgeDsl.g:433:3: ( (lv_isCaseSensitive_2_0= 'isCaseSensitive' ) )?
            int alt11=2;
            int LA11_0 = input.LA(1);

            if ( (LA11_0==22) ) {
                alt11=1;
            }
            switch (alt11) {
                case 1 :
                    // InternalRealmForgeDsl.g:434:4: (lv_isCaseSensitive_2_0= 'isCaseSensitive' )
                    {
                    // InternalRealmForgeDsl.g:434:4: (lv_isCaseSensitive_2_0= 'isCaseSensitive' )
                    // InternalRealmForgeDsl.g:435:5: lv_isCaseSensitive_2_0= 'isCaseSensitive'
                    {
                    lv_isCaseSensitive_2_0=(Token)match(input,22,FOLLOW_16); 

                    					newLeafNode(lv_isCaseSensitive_2_0, grammarAccess.getTypeRacerEventAccess().getIsCaseSensitiveIsCaseSensitiveKeyword_2_0());
                    				

                    					if (current==null) {
                    						current = createModelElement(grammarAccess.getTypeRacerEventRule());
                    					}
                    					setWithLastConsumed(current, "isCaseSensitive", lv_isCaseSensitive_2_0 != null, "isCaseSensitive");
                    				

                    }


                    }
                    break;

            }

            otherlv_3=(Token)match(input,23,FOLLOW_17); 

            			newLeafNode(otherlv_3, grammarAccess.getTypeRacerEventAccess().getDifficultyKeyword_3());
            		
            // InternalRealmForgeDsl.g:451:3: ( (lv_difficulty_4_0= ruleDifficulty ) )
            // InternalRealmForgeDsl.g:452:4: (lv_difficulty_4_0= ruleDifficulty )
            {
            // InternalRealmForgeDsl.g:452:4: (lv_difficulty_4_0= ruleDifficulty )
            // InternalRealmForgeDsl.g:453:5: lv_difficulty_4_0= ruleDifficulty
            {

            					newCompositeNode(grammarAccess.getTypeRacerEventAccess().getDifficultyDifficultyEnumRuleCall_4_0());
            				
            pushFollow(FOLLOW_18);
            lv_difficulty_4_0=ruleDifficulty();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getTypeRacerEventRule());
            					}
            					set(
            						current,
            						"difficulty",
            						lv_difficulty_4_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.Difficulty");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            // InternalRealmForgeDsl.g:470:3: (otherlv_5= 'timeLimit' ( (lv_timeLimit_6_0= ruleEDoubleObject ) ) )?
            int alt12=2;
            int LA12_0 = input.LA(1);

            if ( (LA12_0==24) ) {
                alt12=1;
            }
            switch (alt12) {
                case 1 :
                    // InternalRealmForgeDsl.g:471:4: otherlv_5= 'timeLimit' ( (lv_timeLimit_6_0= ruleEDoubleObject ) )
                    {
                    otherlv_5=(Token)match(input,24,FOLLOW_19); 

                    				newLeafNode(otherlv_5, grammarAccess.getTypeRacerEventAccess().getTimeLimitKeyword_5_0());
                    			
                    // InternalRealmForgeDsl.g:475:4: ( (lv_timeLimit_6_0= ruleEDoubleObject ) )
                    // InternalRealmForgeDsl.g:476:5: (lv_timeLimit_6_0= ruleEDoubleObject )
                    {
                    // InternalRealmForgeDsl.g:476:5: (lv_timeLimit_6_0= ruleEDoubleObject )
                    // InternalRealmForgeDsl.g:477:6: lv_timeLimit_6_0= ruleEDoubleObject
                    {

                    						newCompositeNode(grammarAccess.getTypeRacerEventAccess().getTimeLimitEDoubleObjectParserRuleCall_5_1_0());
                    					
                    pushFollow(FOLLOW_20);
                    lv_timeLimit_6_0=ruleEDoubleObject();

                    state._fsp--;


                    						if (current==null) {
                    							current = createModelElementForParent(grammarAccess.getTypeRacerEventRule());
                    						}
                    						set(
                    							current,
                    							"timeLimit",
                    							lv_timeLimit_6_0,
                    							"no.ntnu.tdt4250.rf.RealmForgeDsl.EDoubleObject");
                    						afterParserOrEnumRuleCall();
                    					

                    }


                    }


                    }
                    break;

            }

            otherlv_7=(Token)match(input,25,FOLLOW_11); 

            			newLeafNode(otherlv_7, grammarAccess.getTypeRacerEventAccess().getRetriesKeyword_6());
            		
            // InternalRealmForgeDsl.g:499:3: ( (lv_retries_8_0= ruleEInt ) )
            // InternalRealmForgeDsl.g:500:4: (lv_retries_8_0= ruleEInt )
            {
            // InternalRealmForgeDsl.g:500:4: (lv_retries_8_0= ruleEInt )
            // InternalRealmForgeDsl.g:501:5: lv_retries_8_0= ruleEInt
            {

            					newCompositeNode(grammarAccess.getTypeRacerEventAccess().getRetriesEIntParserRuleCall_7_0());
            				
            pushFollow(FOLLOW_21);
            lv_retries_8_0=ruleEInt();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getTypeRacerEventRule());
            					}
            					set(
            						current,
            						"retries",
            						lv_retries_8_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EInt");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            otherlv_9=(Token)match(input,26,FOLLOW_3); 

            			newLeafNode(otherlv_9, grammarAccess.getTypeRacerEventAccess().getSentenceKeyword_8());
            		
            // InternalRealmForgeDsl.g:522:3: ( (lv_sentence_10_0= ruleEString ) )
            // InternalRealmForgeDsl.g:523:4: (lv_sentence_10_0= ruleEString )
            {
            // InternalRealmForgeDsl.g:523:4: (lv_sentence_10_0= ruleEString )
            // InternalRealmForgeDsl.g:524:5: lv_sentence_10_0= ruleEString
            {

            					newCompositeNode(grammarAccess.getTypeRacerEventAccess().getSentenceEStringParserRuleCall_9_0());
            				
            pushFollow(FOLLOW_22);
            lv_sentence_10_0=ruleEString();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getTypeRacerEventRule());
            					}
            					set(
            						current,
            						"sentence",
            						lv_sentence_10_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EString");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            otherlv_11=(Token)match(input,27,FOLLOW_14); 

            			newLeafNode(otherlv_11, grammarAccess.getTypeRacerEventAccess().getResultKeyword_10());
            		
            // InternalRealmForgeDsl.g:545:3: ( (lv_result_12_0= ruleEventResult ) )
            // InternalRealmForgeDsl.g:546:4: (lv_result_12_0= ruleEventResult )
            {
            // InternalRealmForgeDsl.g:546:4: (lv_result_12_0= ruleEventResult )
            // InternalRealmForgeDsl.g:547:5: lv_result_12_0= ruleEventResult
            {

            					newCompositeNode(grammarAccess.getTypeRacerEventAccess().getResultEventResultParserRuleCall_11_0());
            				
            pushFollow(FOLLOW_13);
            lv_result_12_0=ruleEventResult();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getTypeRacerEventRule());
            					}
            					set(
            						current,
            						"result",
            						lv_result_12_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EventResult");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            otherlv_13=(Token)match(input,20,FOLLOW_2); 

            			newLeafNode(otherlv_13, grammarAccess.getTypeRacerEventAccess().getRightCurlyBracketKeyword_12());
            		

            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleTypeRacerEvent"


    // $ANTLR start "entryRuleQuestionEvent"
    // InternalRealmForgeDsl.g:572:1: entryRuleQuestionEvent returns [EObject current=null] : iv_ruleQuestionEvent= ruleQuestionEvent EOF ;
    public final EObject entryRuleQuestionEvent() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleQuestionEvent = null;


        try {
            // InternalRealmForgeDsl.g:572:54: (iv_ruleQuestionEvent= ruleQuestionEvent EOF )
            // InternalRealmForgeDsl.g:573:2: iv_ruleQuestionEvent= ruleQuestionEvent EOF
            {
             newCompositeNode(grammarAccess.getQuestionEventRule()); 
            pushFollow(FOLLOW_1);
            iv_ruleQuestionEvent=ruleQuestionEvent();

            state._fsp--;

             current =iv_ruleQuestionEvent; 
            match(input,EOF,FOLLOW_2); 

            }

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleQuestionEvent"


    // $ANTLR start "ruleQuestionEvent"
    // InternalRealmForgeDsl.g:579:1: ruleQuestionEvent returns [EObject current=null] : (otherlv_0= 'QuestionEvent' otherlv_1= '{' otherlv_2= 'difficulty' ( (lv_difficulty_3_0= ruleDifficulty ) ) (otherlv_4= 'timeLimit' ( (lv_timeLimit_5_0= ruleEDoubleObject ) ) )? otherlv_6= 'retries' ( (lv_retries_7_0= ruleEInt ) ) otherlv_8= 'question' ( (lv_question_9_0= ruleEString ) ) otherlv_10= 'result' ( (lv_result_11_0= ruleEventResult ) ) otherlv_12= 'options' otherlv_13= '{' ( (lv_options_14_0= ruleOption ) ) (otherlv_15= ',' ( (lv_options_16_0= ruleOption ) ) )* otherlv_17= '}' otherlv_18= '}' ) ;
    public final EObject ruleQuestionEvent() throws RecognitionException {
        EObject current = null;

        Token otherlv_0=null;
        Token otherlv_1=null;
        Token otherlv_2=null;
        Token otherlv_4=null;
        Token otherlv_6=null;
        Token otherlv_8=null;
        Token otherlv_10=null;
        Token otherlv_12=null;
        Token otherlv_13=null;
        Token otherlv_15=null;
        Token otherlv_17=null;
        Token otherlv_18=null;
        Enumerator lv_difficulty_3_0 = null;

        AntlrDatatypeRuleToken lv_timeLimit_5_0 = null;

        AntlrDatatypeRuleToken lv_retries_7_0 = null;

        AntlrDatatypeRuleToken lv_question_9_0 = null;

        EObject lv_result_11_0 = null;

        EObject lv_options_14_0 = null;

        EObject lv_options_16_0 = null;



        	enterRule();

        try {
            // InternalRealmForgeDsl.g:585:2: ( (otherlv_0= 'QuestionEvent' otherlv_1= '{' otherlv_2= 'difficulty' ( (lv_difficulty_3_0= ruleDifficulty ) ) (otherlv_4= 'timeLimit' ( (lv_timeLimit_5_0= ruleEDoubleObject ) ) )? otherlv_6= 'retries' ( (lv_retries_7_0= ruleEInt ) ) otherlv_8= 'question' ( (lv_question_9_0= ruleEString ) ) otherlv_10= 'result' ( (lv_result_11_0= ruleEventResult ) ) otherlv_12= 'options' otherlv_13= '{' ( (lv_options_14_0= ruleOption ) ) (otherlv_15= ',' ( (lv_options_16_0= ruleOption ) ) )* otherlv_17= '}' otherlv_18= '}' ) )
            // InternalRealmForgeDsl.g:586:2: (otherlv_0= 'QuestionEvent' otherlv_1= '{' otherlv_2= 'difficulty' ( (lv_difficulty_3_0= ruleDifficulty ) ) (otherlv_4= 'timeLimit' ( (lv_timeLimit_5_0= ruleEDoubleObject ) ) )? otherlv_6= 'retries' ( (lv_retries_7_0= ruleEInt ) ) otherlv_8= 'question' ( (lv_question_9_0= ruleEString ) ) otherlv_10= 'result' ( (lv_result_11_0= ruleEventResult ) ) otherlv_12= 'options' otherlv_13= '{' ( (lv_options_14_0= ruleOption ) ) (otherlv_15= ',' ( (lv_options_16_0= ruleOption ) ) )* otherlv_17= '}' otherlv_18= '}' )
            {
            // InternalRealmForgeDsl.g:586:2: (otherlv_0= 'QuestionEvent' otherlv_1= '{' otherlv_2= 'difficulty' ( (lv_difficulty_3_0= ruleDifficulty ) ) (otherlv_4= 'timeLimit' ( (lv_timeLimit_5_0= ruleEDoubleObject ) ) )? otherlv_6= 'retries' ( (lv_retries_7_0= ruleEInt ) ) otherlv_8= 'question' ( (lv_question_9_0= ruleEString ) ) otherlv_10= 'result' ( (lv_result_11_0= ruleEventResult ) ) otherlv_12= 'options' otherlv_13= '{' ( (lv_options_14_0= ruleOption ) ) (otherlv_15= ',' ( (lv_options_16_0= ruleOption ) ) )* otherlv_17= '}' otherlv_18= '}' )
            // InternalRealmForgeDsl.g:587:3: otherlv_0= 'QuestionEvent' otherlv_1= '{' otherlv_2= 'difficulty' ( (lv_difficulty_3_0= ruleDifficulty ) ) (otherlv_4= 'timeLimit' ( (lv_timeLimit_5_0= ruleEDoubleObject ) ) )? otherlv_6= 'retries' ( (lv_retries_7_0= ruleEInt ) ) otherlv_8= 'question' ( (lv_question_9_0= ruleEString ) ) otherlv_10= 'result' ( (lv_result_11_0= ruleEventResult ) ) otherlv_12= 'options' otherlv_13= '{' ( (lv_options_14_0= ruleOption ) ) (otherlv_15= ',' ( (lv_options_16_0= ruleOption ) ) )* otherlv_17= '}' otherlv_18= '}'
            {
            otherlv_0=(Token)match(input,28,FOLLOW_14); 

            			newLeafNode(otherlv_0, grammarAccess.getQuestionEventAccess().getQuestionEventKeyword_0());
            		
            otherlv_1=(Token)match(input,18,FOLLOW_16); 

            			newLeafNode(otherlv_1, grammarAccess.getQuestionEventAccess().getLeftCurlyBracketKeyword_1());
            		
            otherlv_2=(Token)match(input,23,FOLLOW_17); 

            			newLeafNode(otherlv_2, grammarAccess.getQuestionEventAccess().getDifficultyKeyword_2());
            		
            // InternalRealmForgeDsl.g:599:3: ( (lv_difficulty_3_0= ruleDifficulty ) )
            // InternalRealmForgeDsl.g:600:4: (lv_difficulty_3_0= ruleDifficulty )
            {
            // InternalRealmForgeDsl.g:600:4: (lv_difficulty_3_0= ruleDifficulty )
            // InternalRealmForgeDsl.g:601:5: lv_difficulty_3_0= ruleDifficulty
            {

            					newCompositeNode(grammarAccess.getQuestionEventAccess().getDifficultyDifficultyEnumRuleCall_3_0());
            				
            pushFollow(FOLLOW_18);
            lv_difficulty_3_0=ruleDifficulty();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getQuestionEventRule());
            					}
            					set(
            						current,
            						"difficulty",
            						lv_difficulty_3_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.Difficulty");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            // InternalRealmForgeDsl.g:618:3: (otherlv_4= 'timeLimit' ( (lv_timeLimit_5_0= ruleEDoubleObject ) ) )?
            int alt13=2;
            int LA13_0 = input.LA(1);

            if ( (LA13_0==24) ) {
                alt13=1;
            }
            switch (alt13) {
                case 1 :
                    // InternalRealmForgeDsl.g:619:4: otherlv_4= 'timeLimit' ( (lv_timeLimit_5_0= ruleEDoubleObject ) )
                    {
                    otherlv_4=(Token)match(input,24,FOLLOW_19); 

                    				newLeafNode(otherlv_4, grammarAccess.getQuestionEventAccess().getTimeLimitKeyword_4_0());
                    			
                    // InternalRealmForgeDsl.g:623:4: ( (lv_timeLimit_5_0= ruleEDoubleObject ) )
                    // InternalRealmForgeDsl.g:624:5: (lv_timeLimit_5_0= ruleEDoubleObject )
                    {
                    // InternalRealmForgeDsl.g:624:5: (lv_timeLimit_5_0= ruleEDoubleObject )
                    // InternalRealmForgeDsl.g:625:6: lv_timeLimit_5_0= ruleEDoubleObject
                    {

                    						newCompositeNode(grammarAccess.getQuestionEventAccess().getTimeLimitEDoubleObjectParserRuleCall_4_1_0());
                    					
                    pushFollow(FOLLOW_20);
                    lv_timeLimit_5_0=ruleEDoubleObject();

                    state._fsp--;


                    						if (current==null) {
                    							current = createModelElementForParent(grammarAccess.getQuestionEventRule());
                    						}
                    						set(
                    							current,
                    							"timeLimit",
                    							lv_timeLimit_5_0,
                    							"no.ntnu.tdt4250.rf.RealmForgeDsl.EDoubleObject");
                    						afterParserOrEnumRuleCall();
                    					

                    }


                    }


                    }
                    break;

            }

            otherlv_6=(Token)match(input,25,FOLLOW_11); 

            			newLeafNode(otherlv_6, grammarAccess.getQuestionEventAccess().getRetriesKeyword_5());
            		
            // InternalRealmForgeDsl.g:647:3: ( (lv_retries_7_0= ruleEInt ) )
            // InternalRealmForgeDsl.g:648:4: (lv_retries_7_0= ruleEInt )
            {
            // InternalRealmForgeDsl.g:648:4: (lv_retries_7_0= ruleEInt )
            // InternalRealmForgeDsl.g:649:5: lv_retries_7_0= ruleEInt
            {

            					newCompositeNode(grammarAccess.getQuestionEventAccess().getRetriesEIntParserRuleCall_6_0());
            				
            pushFollow(FOLLOW_23);
            lv_retries_7_0=ruleEInt();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getQuestionEventRule());
            					}
            					set(
            						current,
            						"retries",
            						lv_retries_7_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EInt");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            otherlv_8=(Token)match(input,29,FOLLOW_3); 

            			newLeafNode(otherlv_8, grammarAccess.getQuestionEventAccess().getQuestionKeyword_7());
            		
            // InternalRealmForgeDsl.g:670:3: ( (lv_question_9_0= ruleEString ) )
            // InternalRealmForgeDsl.g:671:4: (lv_question_9_0= ruleEString )
            {
            // InternalRealmForgeDsl.g:671:4: (lv_question_9_0= ruleEString )
            // InternalRealmForgeDsl.g:672:5: lv_question_9_0= ruleEString
            {

            					newCompositeNode(grammarAccess.getQuestionEventAccess().getQuestionEStringParserRuleCall_8_0());
            				
            pushFollow(FOLLOW_22);
            lv_question_9_0=ruleEString();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getQuestionEventRule());
            					}
            					set(
            						current,
            						"question",
            						lv_question_9_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EString");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            otherlv_10=(Token)match(input,27,FOLLOW_14); 

            			newLeafNode(otherlv_10, grammarAccess.getQuestionEventAccess().getResultKeyword_9());
            		
            // InternalRealmForgeDsl.g:693:3: ( (lv_result_11_0= ruleEventResult ) )
            // InternalRealmForgeDsl.g:694:4: (lv_result_11_0= ruleEventResult )
            {
            // InternalRealmForgeDsl.g:694:4: (lv_result_11_0= ruleEventResult )
            // InternalRealmForgeDsl.g:695:5: lv_result_11_0= ruleEventResult
            {

            					newCompositeNode(grammarAccess.getQuestionEventAccess().getResultEventResultParserRuleCall_10_0());
            				
            pushFollow(FOLLOW_24);
            lv_result_11_0=ruleEventResult();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getQuestionEventRule());
            					}
            					set(
            						current,
            						"result",
            						lv_result_11_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EventResult");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            otherlv_12=(Token)match(input,30,FOLLOW_14); 

            			newLeafNode(otherlv_12, grammarAccess.getQuestionEventAccess().getOptionsKeyword_11());
            		
            otherlv_13=(Token)match(input,18,FOLLOW_14); 

            			newLeafNode(otherlv_13, grammarAccess.getQuestionEventAccess().getLeftCurlyBracketKeyword_12());
            		
            // InternalRealmForgeDsl.g:720:3: ( (lv_options_14_0= ruleOption ) )
            // InternalRealmForgeDsl.g:721:4: (lv_options_14_0= ruleOption )
            {
            // InternalRealmForgeDsl.g:721:4: (lv_options_14_0= ruleOption )
            // InternalRealmForgeDsl.g:722:5: lv_options_14_0= ruleOption
            {

            					newCompositeNode(grammarAccess.getQuestionEventAccess().getOptionsOptionParserRuleCall_13_0());
            				
            pushFollow(FOLLOW_25);
            lv_options_14_0=ruleOption();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getQuestionEventRule());
            					}
            					add(
            						current,
            						"options",
            						lv_options_14_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.Option");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            // InternalRealmForgeDsl.g:739:3: (otherlv_15= ',' ( (lv_options_16_0= ruleOption ) ) )*
            loop14:
            do {
                int alt14=2;
                int LA14_0 = input.LA(1);

                if ( (LA14_0==31) ) {
                    alt14=1;
                }


                switch (alt14) {
            	case 1 :
            	    // InternalRealmForgeDsl.g:740:4: otherlv_15= ',' ( (lv_options_16_0= ruleOption ) )
            	    {
            	    otherlv_15=(Token)match(input,31,FOLLOW_14); 

            	    				newLeafNode(otherlv_15, grammarAccess.getQuestionEventAccess().getCommaKeyword_14_0());
            	    			
            	    // InternalRealmForgeDsl.g:744:4: ( (lv_options_16_0= ruleOption ) )
            	    // InternalRealmForgeDsl.g:745:5: (lv_options_16_0= ruleOption )
            	    {
            	    // InternalRealmForgeDsl.g:745:5: (lv_options_16_0= ruleOption )
            	    // InternalRealmForgeDsl.g:746:6: lv_options_16_0= ruleOption
            	    {

            	    						newCompositeNode(grammarAccess.getQuestionEventAccess().getOptionsOptionParserRuleCall_14_1_0());
            	    					
            	    pushFollow(FOLLOW_25);
            	    lv_options_16_0=ruleOption();

            	    state._fsp--;


            	    						if (current==null) {
            	    							current = createModelElementForParent(grammarAccess.getQuestionEventRule());
            	    						}
            	    						add(
            	    							current,
            	    							"options",
            	    							lv_options_16_0,
            	    							"no.ntnu.tdt4250.rf.RealmForgeDsl.Option");
            	    						afterParserOrEnumRuleCall();
            	    					

            	    }


            	    }


            	    }
            	    break;

            	default :
            	    break loop14;
                }
            } while (true);

            otherlv_17=(Token)match(input,20,FOLLOW_13); 

            			newLeafNode(otherlv_17, grammarAccess.getQuestionEventAccess().getRightCurlyBracketKeyword_15());
            		
            otherlv_18=(Token)match(input,20,FOLLOW_2); 

            			newLeafNode(otherlv_18, grammarAccess.getQuestionEventAccess().getRightCurlyBracketKeyword_16());
            		

            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleQuestionEvent"


    // $ANTLR start "entryRuleOption"
    // InternalRealmForgeDsl.g:776:1: entryRuleOption returns [EObject current=null] : iv_ruleOption= ruleOption EOF ;
    public final EObject entryRuleOption() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleOption = null;


        try {
            // InternalRealmForgeDsl.g:776:47: (iv_ruleOption= ruleOption EOF )
            // InternalRealmForgeDsl.g:777:2: iv_ruleOption= ruleOption EOF
            {
             newCompositeNode(grammarAccess.getOptionRule()); 
            pushFollow(FOLLOW_1);
            iv_ruleOption=ruleOption();

            state._fsp--;

             current =iv_ruleOption; 
            match(input,EOF,FOLLOW_2); 

            }

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleOption"


    // $ANTLR start "ruleOption"
    // InternalRealmForgeDsl.g:783:1: ruleOption returns [EObject current=null] : (otherlv_0= '{' otherlv_1= 'text' ( (lv_text_2_0= ruleEString ) ) ( (lv_isCorrectAnswer_3_0= 'isCorrectAnswer' ) )? otherlv_4= '}' ) ;
    public final EObject ruleOption() throws RecognitionException {
        EObject current = null;

        Token otherlv_0=null;
        Token otherlv_1=null;
        Token lv_isCorrectAnswer_3_0=null;
        Token otherlv_4=null;
        AntlrDatatypeRuleToken lv_text_2_0 = null;



        	enterRule();

        try {
            // InternalRealmForgeDsl.g:789:2: ( (otherlv_0= '{' otherlv_1= 'text' ( (lv_text_2_0= ruleEString ) ) ( (lv_isCorrectAnswer_3_0= 'isCorrectAnswer' ) )? otherlv_4= '}' ) )
            // InternalRealmForgeDsl.g:790:2: (otherlv_0= '{' otherlv_1= 'text' ( (lv_text_2_0= ruleEString ) ) ( (lv_isCorrectAnswer_3_0= 'isCorrectAnswer' ) )? otherlv_4= '}' )
            {
            // InternalRealmForgeDsl.g:790:2: (otherlv_0= '{' otherlv_1= 'text' ( (lv_text_2_0= ruleEString ) ) ( (lv_isCorrectAnswer_3_0= 'isCorrectAnswer' ) )? otherlv_4= '}' )
            // InternalRealmForgeDsl.g:791:3: otherlv_0= '{' otherlv_1= 'text' ( (lv_text_2_0= ruleEString ) ) ( (lv_isCorrectAnswer_3_0= 'isCorrectAnswer' ) )? otherlv_4= '}'
            {
            otherlv_0=(Token)match(input,18,FOLLOW_26); 

            			newLeafNode(otherlv_0, grammarAccess.getOptionAccess().getLeftCurlyBracketKeyword_0());
            		
            otherlv_1=(Token)match(input,32,FOLLOW_3); 

            			newLeafNode(otherlv_1, grammarAccess.getOptionAccess().getTextKeyword_1());
            		
            // InternalRealmForgeDsl.g:799:3: ( (lv_text_2_0= ruleEString ) )
            // InternalRealmForgeDsl.g:800:4: (lv_text_2_0= ruleEString )
            {
            // InternalRealmForgeDsl.g:800:4: (lv_text_2_0= ruleEString )
            // InternalRealmForgeDsl.g:801:5: lv_text_2_0= ruleEString
            {

            					newCompositeNode(grammarAccess.getOptionAccess().getTextEStringParserRuleCall_2_0());
            				
            pushFollow(FOLLOW_27);
            lv_text_2_0=ruleEString();

            state._fsp--;


            					if (current==null) {
            						current = createModelElementForParent(grammarAccess.getOptionRule());
            					}
            					set(
            						current,
            						"text",
            						lv_text_2_0,
            						"no.ntnu.tdt4250.rf.RealmForgeDsl.EString");
            					afterParserOrEnumRuleCall();
            				

            }


            }

            // InternalRealmForgeDsl.g:818:3: ( (lv_isCorrectAnswer_3_0= 'isCorrectAnswer' ) )?
            int alt15=2;
            int LA15_0 = input.LA(1);

            if ( (LA15_0==33) ) {
                alt15=1;
            }
            switch (alt15) {
                case 1 :
                    // InternalRealmForgeDsl.g:819:4: (lv_isCorrectAnswer_3_0= 'isCorrectAnswer' )
                    {
                    // InternalRealmForgeDsl.g:819:4: (lv_isCorrectAnswer_3_0= 'isCorrectAnswer' )
                    // InternalRealmForgeDsl.g:820:5: lv_isCorrectAnswer_3_0= 'isCorrectAnswer'
                    {
                    lv_isCorrectAnswer_3_0=(Token)match(input,33,FOLLOW_13); 

                    					newLeafNode(lv_isCorrectAnswer_3_0, grammarAccess.getOptionAccess().getIsCorrectAnswerIsCorrectAnswerKeyword_3_0());
                    				

                    					if (current==null) {
                    						current = createModelElement(grammarAccess.getOptionRule());
                    					}
                    					setWithLastConsumed(current, "isCorrectAnswer", lv_isCorrectAnswer_3_0 != null, "isCorrectAnswer");
                    				

                    }


                    }
                    break;

            }

            otherlv_4=(Token)match(input,20,FOLLOW_2); 

            			newLeafNode(otherlv_4, grammarAccess.getOptionAccess().getRightCurlyBracketKeyword_4());
            		

            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleOption"


    // $ANTLR start "ruleDifficulty"
    // InternalRealmForgeDsl.g:840:1: ruleDifficulty returns [Enumerator current=null] : ( (enumLiteral_0= 'EASY' ) | (enumLiteral_1= 'NORMAL' ) | (enumLiteral_2= 'HARD' ) ) ;
    public final Enumerator ruleDifficulty() throws RecognitionException {
        Enumerator current = null;

        Token enumLiteral_0=null;
        Token enumLiteral_1=null;
        Token enumLiteral_2=null;


        	enterRule();

        try {
            // InternalRealmForgeDsl.g:846:2: ( ( (enumLiteral_0= 'EASY' ) | (enumLiteral_1= 'NORMAL' ) | (enumLiteral_2= 'HARD' ) ) )
            // InternalRealmForgeDsl.g:847:2: ( (enumLiteral_0= 'EASY' ) | (enumLiteral_1= 'NORMAL' ) | (enumLiteral_2= 'HARD' ) )
            {
            // InternalRealmForgeDsl.g:847:2: ( (enumLiteral_0= 'EASY' ) | (enumLiteral_1= 'NORMAL' ) | (enumLiteral_2= 'HARD' ) )
            int alt16=3;
            switch ( input.LA(1) ) {
            case 34:
                {
                alt16=1;
                }
                break;
            case 35:
                {
                alt16=2;
                }
                break;
            case 36:
                {
                alt16=3;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 16, 0, input);

                throw nvae;
            }

            switch (alt16) {
                case 1 :
                    // InternalRealmForgeDsl.g:848:3: (enumLiteral_0= 'EASY' )
                    {
                    // InternalRealmForgeDsl.g:848:3: (enumLiteral_0= 'EASY' )
                    // InternalRealmForgeDsl.g:849:4: enumLiteral_0= 'EASY'
                    {
                    enumLiteral_0=(Token)match(input,34,FOLLOW_2); 

                    				current = grammarAccess.getDifficultyAccess().getEASYEnumLiteralDeclaration_0().getEnumLiteral().getInstance();
                    				newLeafNode(enumLiteral_0, grammarAccess.getDifficultyAccess().getEASYEnumLiteralDeclaration_0());
                    			

                    }


                    }
                    break;
                case 2 :
                    // InternalRealmForgeDsl.g:856:3: (enumLiteral_1= 'NORMAL' )
                    {
                    // InternalRealmForgeDsl.g:856:3: (enumLiteral_1= 'NORMAL' )
                    // InternalRealmForgeDsl.g:857:4: enumLiteral_1= 'NORMAL'
                    {
                    enumLiteral_1=(Token)match(input,35,FOLLOW_2); 

                    				current = grammarAccess.getDifficultyAccess().getNORMALEnumLiteralDeclaration_1().getEnumLiteral().getInstance();
                    				newLeafNode(enumLiteral_1, grammarAccess.getDifficultyAccess().getNORMALEnumLiteralDeclaration_1());
                    			

                    }


                    }
                    break;
                case 3 :
                    // InternalRealmForgeDsl.g:864:3: (enumLiteral_2= 'HARD' )
                    {
                    // InternalRealmForgeDsl.g:864:3: (enumLiteral_2= 'HARD' )
                    // InternalRealmForgeDsl.g:865:4: enumLiteral_2= 'HARD'
                    {
                    enumLiteral_2=(Token)match(input,36,FOLLOW_2); 

                    				current = grammarAccess.getDifficultyAccess().getHARDEnumLiteralDeclaration_2().getEnumLiteral().getInstance();
                    				newLeafNode(enumLiteral_2, grammarAccess.getDifficultyAccess().getHARDEnumLiteralDeclaration_2());
                    			

                    }


                    }
                    break;

            }


            }


            	leaveRule();

        }

            catch (RecognitionException re) {
                recover(input,re);
                appendSkippedTokens();
            }
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleDifficulty"

    // Delegated rules


 

    public static final BitSet FOLLOW_1 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_2 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_3 = new BitSet(new long[]{0x0000000000000030L});
    public static final BitSet FOLLOW_4 = new BitSet(new long[]{0x0000000000001000L});
    public static final BitSet FOLLOW_5 = new BitSet(new long[]{0x0000000000002000L});
    public static final BitSet FOLLOW_6 = new BitSet(new long[]{0x0000000010200002L});
    public static final BitSet FOLLOW_7 = new BitSet(new long[]{0x0000000000008040L});
    public static final BitSet FOLLOW_8 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_9 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_10 = new BitSet(new long[]{0x0000000000030002L});
    public static final BitSet FOLLOW_11 = new BitSet(new long[]{0x0000000000004040L});
    public static final BitSet FOLLOW_12 = new BitSet(new long[]{0x0000000000180000L});
    public static final BitSet FOLLOW_13 = new BitSet(new long[]{0x0000000000100000L});
    public static final BitSet FOLLOW_14 = new BitSet(new long[]{0x0000000000040000L});
    public static final BitSet FOLLOW_15 = new BitSet(new long[]{0x0000000000C00000L});
    public static final BitSet FOLLOW_16 = new BitSet(new long[]{0x0000000000800000L});
    public static final BitSet FOLLOW_17 = new BitSet(new long[]{0x0000001C00000000L});
    public static final BitSet FOLLOW_18 = new BitSet(new long[]{0x0000000003000000L});
    public static final BitSet FOLLOW_19 = new BitSet(new long[]{0x000000000000C040L});
    public static final BitSet FOLLOW_20 = new BitSet(new long[]{0x0000000002000000L});
    public static final BitSet FOLLOW_21 = new BitSet(new long[]{0x0000000004000000L});
    public static final BitSet FOLLOW_22 = new BitSet(new long[]{0x0000000008000000L});
    public static final BitSet FOLLOW_23 = new BitSet(new long[]{0x0000000020000000L});
    public static final BitSet FOLLOW_24 = new BitSet(new long[]{0x0000000040000000L});
    public static final BitSet FOLLOW_25 = new BitSet(new long[]{0x0000000080100000L});
    public static final BitSet FOLLOW_26 = new BitSet(new long[]{0x0000000100000000L});
    public static final BitSet FOLLOW_27 = new BitSet(new long[]{0x0000000200100000L});

}