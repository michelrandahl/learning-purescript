import chalk from 'chalk';

export const _chalk = (styles, str) => {
  return styles.reduce((acc,style) => acc[style], chalk)(str);
};
