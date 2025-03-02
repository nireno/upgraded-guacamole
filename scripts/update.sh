pm2 stop allfours \
&& git pull \
&& npm install \
&& npm run build \
&& pm2 start allfours
