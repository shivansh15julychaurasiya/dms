import React from 'react';
import { Nav, NavItem, NavLink } from 'reactstrap';
import { useNavigate } from 'react-router-dom';

export default function Sidebar({ collapsed, isMobileOpen, setIsMobileOpen }) {
  const navigate = useNavigate();

  const sidebarClass = `sidebar d-flex flex-column p-3 
    ${collapsed ? 'collapsed' : ''} 
    ${isMobileOpen ? 'mobile-open' : ''}`;

  const handleNavClick = (path) => {
    navigate(path);
    if (window.innerWidth < 992) setIsMobileOpen(false); // auto-close on mobile
  };

  return (
    <div className={sidebarClass}>
      <h4 className="text-white text-center mb-4">{collapsed ? 'MP' : 'MyPanel'}</h4>

      <Nav vertical pills>
        <NavItem>
          <NavLink className="text-white" onClick={() => handleNavClick('/dashboard')}>
            🏠 {!collapsed && 'Dashboard'}
          </NavLink>
        </NavItem>
        <NavItem>
          <NavLink className="text-white" onClick={() => handleNavClick('/users')}>
            👥 {!collapsed && 'Users'}
          </NavLink>
        </NavItem>
         <NavItem>
          <NavLink className="text-white" onClick={() => handleNavClick('/products')}>
            👥 {!collapsed && 'Products '}
          </NavLink>
        </NavItem>
         <NavItem>
          <NavLink className="text-white" onClick={() => handleNavClick('/categories')}>
            👥 {!collapsed && 'Categories'}
          </NavLink>
        </NavItem>
         <NavItem>
          <NavLink className="text-white" onClick={() => handleNavClick('/subcategories')}>
            👥 {!collapsed && 'SubCategories'}
          </NavLink>
        </NavItem>
        <NavItem>
          <NavLink className="text-white" onClick={() => handleNavClick('/settings')}>
            ⚙️ {!collapsed && 'Settings'}
          </NavLink>
        </NavItem>
      </Nav>
    </div>
  );
}
