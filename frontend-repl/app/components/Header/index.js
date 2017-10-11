import React from 'react';
import { FormattedMessage } from 'react-intl';

import A from './A';
import Img from './Img';
import NavBar from './NavBar';
import HeaderLink from './HeaderLink';
import Banner from './banner.jpg';
import messages from './messages';

class Header extends React.Component {
  render() {
    return (
      <div>
        <NavBar>
        </NavBar>
      </div>
    );
  }
}

export default Header;
