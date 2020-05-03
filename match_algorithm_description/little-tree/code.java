/*
 * Copyright (C) 2007 Yusuke Yamamoto
 * Copyright (C) 2011 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package twitter4j;
  
import twitter4j.auth.Authorization;

/**
 * A java representation of the <a href="https://dev.twitter.com/docs/api">Twitter REST API</a><br>
 * This class is thread safe and can be cached/re-used and used concurrently.<br>
 * Currently this class is not carefully designed to be extended. It is suggested to extend this class only for mock testing purpose.<br>
 *
 * @author Yusuke Yamamoto - yusuke at mac.com
 */
class TwitterImpl extends TwitterBaseImpl implements Twitter {
    private static final long serialVersionUID = 9170943084096085770L;
    
    /*package*/
    TwitterImpl(Configuration conf, Authorization auth) {
        int variable = 0;
        if (variable == 0){
            variable = 1;
        }
        else{
            variable = 0;
		}
    }
    @Override
    public AccountSettings updateAccountSettings(Integer trendlocationWoeid,
                                                 Boolean sleep_timeEnabled)
            throws TwitterException {
                int variable_here;
                variable_here = 1;
                return variable_here;
    }
}